{-# LANGUAGE Arrows, NamedFieldPuns #-}
module Scoutess.DataFlow where

import Control.Applicative                   ((<$>))
import Control.Arrow
import Control.Monad                         (filterM, when)
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
import System.Directory                      (createDirectoryIfMissing, doesDirectoryExist, removeDirectoryRecursive)
import System.FilePath                       ((</>),(<.>))

import Scoutess.Core
import Scoutess.Service.LocalHackage.Core
import Scoutess.Service.Source.Fetch         (fetchSrcs, fetchVersion, fetchVersions)

-- standard build
standard :: Scoutess SourceSpec SourceSpec    -- ^ sourceFilter
         -> Scoutess VersionSpec VersionSpec  -- ^ versionFilter
         -> Scoutess (SourceSpec, TargetSpec, Maybe PriorRun) BuildReport
standard sourceFilter versionFilter =
    proc (sourceSpec, targetSpec, priorRun) ->
        do (availableVersions, targetVersion)
                              <- fetchVersionSpec sourceFilter -< (targetSpec, sourceSpec)
           consideredVersions <- versionFilter                 -< availableVersions
           dependencyGraph    <- calculateDependencies         -< (targetSpec, targetVersion, consideredVersions)
           buildSpec          <- calculateChanges              -< (targetSpec, priorRun, dependencyGraph)
           hackageIndex       <- updateLocalHackage            -< buildSpec
           buildReport        <- build                         -< (hackageIndex, buildSpec)
           returnA -< buildReport

-- TODO: test and handle errors
-- | fetch the 'VersionSpec' for the dependencies and the 'VersionInfo' for the target package.
--   Note that the dependencies are fetched from the filtered 'SourceSpec' while the target is
--   from the unfiltered 'SourceSpec'.
fetchVersionSpec :: Scoutess SourceSpec SourceSpec -> Scoutess (TargetSpec, SourceSpec) (VersionSpec, VersionInfo)
fetchVersionSpec sourceFilter = liftScoutess $ \(targetSpec, sourceSpec) -> do
    let (name, version, location) = tsNameVersionLocation targetSpec
    filteredSources <- S.toList . locations <$> runScoutess sourceFilter sourceSpec
    if location `elem` filteredSources
      then do
        (errors, versionSpecs) <- fetchVersions (tsSourceConfig targetSpec) filteredSources
        let combined = combineVersionSpecs versionSpecs
        let Just targetVersion = findVersion name version location combined
        return (combined, targetVersion)
      else do
        (errors, versionSpecs) <- fetchVersions (tsSourceConfig targetSpec) filteredSources
        eitherTargetVersionSpec <- fetchVersion (tsSourceConfig targetSpec) location
        let Just targetVersion = either (error "could not find the target") (findVersion name version location) eitherTargetVersionSpec
        return (combineVersionSpecs versionSpecs, targetVersion)

calculateDependencies :: Scoutess (TargetSpec, VersionInfo, VersionSpec) DependencyGraph
calculateDependencies = liftScoutess $ \(targetSpec, targetVersion, versionSpec) -> do
    let (depMap, bimap) = runState (dependencyMap versionSpec targetVersion) B.empty
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
getVersionIndex :: VersionInfo -> State (Bimap Vertex VersionInfo) Vertex
getVersionIndex version = do
    bimap <- get
    let addNew = do
          let ix = B.size bimap
          put $ B.insert ix version bimap
          return ix
    maybe addNew return (B.lookupR version bimap)

-- | Adds the 'VersionInfo' and its dependencies to the 'Bimap', then returns a 'Map' from the vertex for each
--   package to the vertices of its dependencies.
dependencyMap :: VersionSpec -> VersionInfo
              -> State (Bimap Vertex VersionInfo) (Map Vertex [Vertex])
dependencyMap spec version = do
    let deps = getImmDeps spec version
    unseenDeps <- filterM (gets . B.notMemberR) deps
    index      <- getVersionIndex version
    depIndices <- mapM getVersionIndex deps
    depMaps    <- mapM (dependencyMap spec) unseenDeps
    return $ M.insert index depIndices (M.unions depMaps)

calculateChanges :: Scoutess (TargetSpec, Maybe PriorRun, DependencyGraph) BuildSpec
calculateChanges = liftScoutess $ \(targetSpec, mPriorRun, depGraph) -> do
    let allPackages = S.fromAscList . map fst . B.toAscListR . association $ depGraph
        newPackages = maybe allPackages (const err) mPriorRun
        err         = error "PriorRun not yet implemented"
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
    return $ LocalHackageIndex (indexPath <.> ".gz")

-- | sandboxing doesn't seem to work - user package-db was still recognised by cabal
build :: Scoutess (LocalHackageIndex, BuildSpec) BuildReport
build = liftScoutess $ \(localHackageIndex, buildSpec) -> do
    let targetSpec  = bsTargetSpec buildSpec
        sandboxDir  = tsPackageDB targetSpec
        installDir  = sandboxDir
        configFile  = tsTmpDir targetSpec </> "config"
        targetCabal = undefined
    dirExists <- doesDirectoryExist sandboxDir
    when (dirExists) (removeDirectoryRecursive sandboxDir)
    (ghcPkgExitCode, ghcPkgOut, ghcPkgErr) <-
        readProcessWithExitCode "ghc-pkg" ["init", sandboxDir] []
    writeFile configFile $ unlines
      [ "local-repo: " ++ hackageDir (tsLocalHackage (bsTargetSpec buildSpec))
      , "build-summary: " ++ (tsTmpDir (bsTargetSpec buildSpec) </> "build.log")
      , "remote-build-reporting: anonymous"]
    -- TODO: allow custom cabal args
    let cabalArgs = ["--config-file=" ++ configFile
                    ,"install"
                    ,targetCabal
                    ,"--package-db=clear"
                    ,"--package-db=" ++ sandboxDir
                    ,"--prefix=" ++ installDir]
    (cabalExitCode, cabalOut, cabalErr) <- readProcessWithExitCode "cabal" cabalArgs []
    let buildReport = undefined
    -- TODO: collect cabal's build report and the results of other processes
    return buildReport