{-# LANGUAGE Arrows, NamedFieldPuns #-}
module Scoutess.DataFlow where

import Control.Applicative                   ((<$>))
import Control.Arrow
import Control.Monad                         (filterM, (<=<))
import Control.Monad.State                   (State, runState, get, put, gets)
import Data.Array                            (Array, array)
import Data.Bimap                            (Bimap)
import qualified Data.Bimap as B
import Data.Graph                            (Vertex)
import Data.Map                              (Map)
import qualified Data.Map as M
import Data.Maybe                            (catMaybes)
import Data.Set                              (Set)
import qualified Data.Set as S
import Distribution.Package
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Verbosity                (silent)
import Distribution.Version                  (withinRange)
import System.Process                        (readProcessWithExitCode)
import System.Directory                      (createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath                       ((</>),(<.>))

import Scoutess.Core
import Scoutess.Service.LocalHackage.Core
import Scoutess.Service.Source.Fetch         (fetchSrcs, fetchVersions)

-- standard build
standard :: Scoutess SourceSpec SourceSpec    -- ^ sourceFilter
         -> Scoutess VersionSpec VersionSpec  -- ^ versionFilter
         -> Scoutess (SourceSpec, TargetSpec, Maybe PriorRun) BuildReport
standard sourceFilter versionFilter =
    proc (sourceSpec, targetSpec, priorRun) ->
        do availableVersions  <- fetchVersions' <<< second sourceFilter -< (targetSpec, sourceSpec)
           consideredVersions <- versionFilter                         -< availableVersions
           dependencyGraph    <- calculateDependencies                 -< (targetSpec, consideredVersions)
           buildSpec          <- calculateChanges                      -< (targetSpec, priorRun, dependencyGraph)
           hackageIndex       <- updateLocalHackage                    -< buildSpec
           buildReport        <- build                                 -< (hackageIndex, buildSpec)
           returnA -< buildReport

fetchVersions' :: Scoutess (TargetSpec, SourceSpec) VersionSpec
fetchVersions' = liftScoutess $ \(targetSpec, sourceSpec) -> do
    let locations' = S.toList (locations sourceSpec)
    (errors, versionss) <- fetchVersions (tsSourceConfig targetSpec) locations'
    return . VersionSpec . S.unions $ versionss

calculateDependencies :: Scoutess (TargetSpec, VersionSpec) DependencyGraph
calculateDependencies = liftScoutess $ \(targetSpec, versionSpec) -> do
    cabalFile <- findCabalFile (tsSourceDir targetSpec)
    gpd <- readPackageDescription silent cabalFile
    let targetVersion   = createVersionInfo (Dir (tsSourceDir targetSpec)) gpd
        (depMap, bimap) = runState (dependencyMap versionSpec targetVersion) B.empty
        bounds          = (0, B.size bimap -1)
        depArr         :: Array Vertex [Vertex]
        depArr          = array bounds (M.toList depMap)
    return DependencyGraph {graph = depArr, association = bimap}

-- | Return the immediate dependencies of a given 'VersionInfo'
--   currently takes the highest valid dependency.
--   XXX: if a dependency can't be found, it is silently
--   ignored (cabal will then throw an error when compiling)
getImmDeps :: VersionSpec -> VersionInfo -> [VersionInfo]
getImmDeps versionSpec = catMaybes . map findDep . viDependencies
    where
    findDep :: Dependency -> Maybe VersionInfo
    findDep dep = fst <$> S.maxView (S.filter (fitsDep dep) (versions versionSpec))
    fitsDep :: Dependency -> VersionInfo -> Bool
    fitsDep (Dependency (PackageName name) range) vi =
        (viName vi == name) && (viVersion vi `withinRange` range)

-- | Find the index of a 'VersionInfo' (adding it to the 'Bimap' if it isn't found).
getOrAddVersionIndex :: VersionInfo -> State (Bimap Vertex VersionInfo) Vertex
getOrAddVersionIndex version = do
    bimap <- get
    let addNew = do
          let ix = B.size bimap
          put $ B.insert ix version bimap
          return ix
    maybe addNew return (B.lookupR version bimap)

-- | Adds the 'VersionInfo''s dependencies to the 'Bimap', then returns a dependency 'Map'.
--   Note that the 'VersionInfo' itself is not in either the bimap or map.
dependencyMap :: VersionSpec -> VersionInfo
              -> State (Bimap Vertex VersionInfo) (Map Vertex [Vertex])
dependencyMap spec version = do
    let deps = getImmDeps spec version
    M.unions <$> mapM (dependencyMap' spec) deps

dependencyMap' :: VersionSpec -> VersionInfo
               -> State (Bimap Vertex VersionInfo) (Map Vertex [Vertex])
dependencyMap' spec version = do
    let deps = getImmDeps spec version
    deps'    <- filterM (gets . B.notMemberR) deps
    versionI <- getOrAddVersionIndex version
    depsI    <- mapM getOrAddVersionIndex deps
    depsM    <- mapM (dependencyMap' spec) deps'
    return $ M.insert versionI depsI (M.unions depsM)

calculateChanges :: Scoutess (TargetSpec, Maybe PriorRun, DependencyGraph) BuildSpec
calculateChanges = liftScoutess $ \(targetSpec, mPriorRun, depGraph) -> do
    let allPackages = S.fromAscList . map fst . B.toAscListR . association $ depGraph
        newPackages = case mPriorRun of
            Just priorRun -> error "PriorRun not yet implemented"
            Nothing       -> allPackages
    return BuildSpec
      { bsTargetSpec = targetSpec
      , bsNewDeps    = newPackages
      , bsAllDeps    = allPackages}

updateLocalHackage :: Scoutess BuildSpec LocalHackageIndex
updateLocalHackage = liftScoutess $ \buildSpec -> do
    let localHackage = tsLocalHackage (bsTargetSpec buildSpec)
        tmpDir       = tsTmpDir (bsTargetSpec buildSpec)
        indexPath    = tmpDir </> "00-index.tar"
        sourceConfig = tsSourceConfig (bsTargetSpec buildSpec)
    createDirectoryIfMissing True tmpDir
    (errors, sourceInfos) <- fetchSrcs sourceConfig (S.toList (bsNewDeps buildSpec))
    mapM_ (flip addPackage localHackage) sourceInfos
    generateIndexSelectively (Just . S.toList . bsAllDeps $ buildSpec) localHackage indexPath
    -- TODO: give this index to cabal without disrupting the main index in the repo
    return $ LocalHackageIndex (indexPath <.> ".gz")

-- | sandboxing doesn't seem to work - user package-db was still recognised by cabal
build :: Scoutess (LocalHackageIndex, BuildSpec) BuildReport
build = liftScoutess $ \(localHackageIndex, buildSpec) -> do
    let targetSpec  = bsTargetSpec buildSpec
        sandboxDir  = tsPackageDB targetSpec
        configFile  = tsTmpDir targetSpec </> "config"
    cabalFile <- findCabalFile (tsSourceDir targetSpec)
    dirExists <- doesDirectoryExist sandboxDir
    (ghcPkgExitCode, ghcPkgOut, ghcPkgErr) <- if dirExists
        then error $ "Scoutess isn't currently caching from previous runs, please delete "
                 ++ "\"" ++ sandboxDir ++ "\" and run again."
        else readProcessWithExitCode "ghc-pkg" ["init", "\"" ++ sandboxDir ++ "\""] []
    writeFile configFile $ unlines
      [ "local-repo: " ++ hackageDir (tsLocalHackage (bsTargetSpec buildSpec))
      , "build-summary: " ++ (tsTmpDir (bsTargetSpec buildSpec) </> "build.log")
      , "remote-build-reporting: anonymous"]
    -- TODO: allow custom cabal args
    let cabalArgs = ["--config-file=\"" ++ configFile ++ "\""
                    ,"install"
                    ,"\"" ++ cabalFile ++ "\""
                    ,"--package-db=clear"
                    ,"--package-db=\"" ++ sandboxDir ++ "\""
                    ,"--prefix=\"" ++ sandboxDir ++ "\""]
    (cabalExitCode, cabalOut, cabalErr) <- readProcessWithExitCode "cabal" cabalArgs []
    let buildReport = undefined
    -- TODO: collect cabal's build report and the results of other processes
    return buildReport