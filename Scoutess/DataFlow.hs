{-# LANGUAGE Arrows #-}
module Scoutess.DataFlow where

import Control.Applicative                   ((<$>))
import Control.Arrow
import Control.Monad                         (filterM, when, forM_)
import Control.Monad.State                   (State, runState, get, put, gets)
import Data.Array                            (Array, array)
import Data.Bimap                            (Bimap)
import qualified Data.Bimap as B
import Data.Function                         (on)
import Data.Graph                            (Vertex)
import Data.Map                              (Map)
import qualified Data.Map as M
import Data.Maybe                            (catMaybes, fromMaybe)
import Data.Set                              (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Distribution.Package
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Verbosity                (silent)
import Distribution.Version                  (withinRange)
import System.Exit                           (ExitCode(..))
import System.Process                        (readProcessWithExitCode)
import System.Directory                      (createDirectory, createDirectoryIfMissing, doesDirectoryExist, removeDirectoryRecursive)
import System.FilePath                       ((</>),(<.>))

import Scoutess.Core
import Scoutess.Service.LocalHackage.Core
import Scoutess.Service.Source.Fetch         (fetchSrc, fetchSrcs, fetchVersion, fetchVersions)

-- standard build
standard :: Scoutess SourceSpec SourceSpec    -- ^ sourceFilter
         -> Scoutess VersionSpec VersionSpec  -- ^ versionFilter
         -> Scoutess (SourceSpec, TargetSpec, Maybe PriorRun) BuildReport
standard sourceFilter versionFilter = proc (sourceSpec, targetSpec, mPriorRun) -> do
    initialise -< targetSpec
    (availableVersions, targetVersion, versionExceptions)
                         <- fetchVersionSpec sourceFilter -< (targetSpec, sourceSpec)
    consideredVersions   <- versionFilter                 -< availableVersions
    dependencyGraph      <- calculateDependencies         -< (targetSpec, targetVersion, consideredVersions)
    buildSpec            <- calculateChanges              -< (targetSpec, mPriorRun, dependencyGraph)
    if bsToBuild buildSpec
    then do
        (buildSources, sourceExceptions)
                     <- getSources                    -< (buildSpec, targetVersion)
        hackageIndex <- updateLocalHackage            -< (buildSpec, buildSources)
        exceptions   <- recordExceptions              -< (versionExceptions, Just sourceExceptions)
        build                                         -< (buildSpec, hackageIndex, targetSource buildSources, exceptions)
    else do
        exceptions   <- recordExceptions              -< (versionExceptions, Nothing)
        -- we didn't build but still return the target and depGraph
        notBuild                                      -< (buildSpec, targetVersion, exceptions)

-- ensure that the temp, localHackage and package-db directories exist and are empty
initialise :: Scoutess TargetSpec ()
initialise = liftScoutess $ \targetSpec -> do
    clearLocalHackage (tsLocalHackage targetSpec)
    forM_ [tsTmpDir targetSpec] $ \filePath -> do
        dirExists <- doesDirectoryExist filePath
        when dirExists (removeDirectoryRecursive filePath)
        createDirectoryIfMissing True filePath
    pkgDirExists <- doesDirectoryExist (tsPackageDB targetSpec)
    when pkgDirExists (removeDirectoryRecursive (tsPackageDB targetSpec))

-- | fetch the 'VersionSpec' for the dependencies and the 'VersionInfo' for the target package.
--   Note that the dependencies are fetched from the filtered 'SourceSpec' while the target is
--   from the unfiltered 'SourceSpec'.
fetchVersionSpec :: Scoutess SourceSpec SourceSpec -> Scoutess (TargetSpec, SourceSpec) (VersionSpec, VersionInfo, Set SourceException)
fetchVersionSpec sourceFilter = liftScoutess $ \(targetSpec, sourceSpec) -> do
    let (name, version, location) = tsNameVersionLocation targetSpec
    filteredSources <- S.toList . locations <$> runScoutess sourceFilter sourceSpec
    (exceptions, versionSpecs) <- fetchVersions (tsSourceConfig targetSpec) filteredSources
    let combined = combineVersionSpecs versionSpecs
    mTargetVersion <- if location `elem` filteredSources
        then return $ findVersion name version location combined
        else either (error . T.unpack . sourceErrorMsg) (findVersion name version location) <$> fetchVersion (tsSourceConfig targetSpec) location
    let targetVersion = fromMaybe (error "could not find the target in the sources") mTargetVersion
    return (combined, targetVersion, S.fromList exceptions)

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

-- | If the target, the dependencies and all their versions are the same as before, we don't need to build.
calculateChanges :: Scoutess (TargetSpec, Maybe PriorRun, DependencyGraph) BuildSpec
calculateChanges = liftScoutess $ \(targetSpec, mPriorRun, depGraph) -> return $
    let toBuild = not . ((==) `on` dgVersionInfos) depGraph . prDepGraph
    in BuildSpec targetSpec depGraph mPriorRun (maybe True toBuild mPriorRun)

-- | Fetch the sources for the target and its dependencies
getSources :: Scoutess (BuildSpec, VersionInfo) (BuildSources, Set SourceException)
getSources = liftScoutess $ \(buildSpec, targetVI) -> do
    let sourceConfig      = tsSourceConfig (bsTargetSpec buildSpec)
        depVersionInfos   = targetVI `S.delete` dgVersionInfos (bsDepGraph buildSpec)
    targetSource <- either (error . T.unpack . sourceErrorMsg) id <$> fetchSrc sourceConfig targetVI
    (exceptions, depSources) <- fetchSrcs sourceConfig (S.toList depVersionInfos)
    return (BuildSources targetSource (S.fromList depSources), S.fromList exceptions)

-- | Add all the sources to the local hackage index and write a hackage index containing only those package to the temp directory
updateLocalHackage :: Scoutess (BuildSpec, BuildSources) LocalHackageIndex
updateLocalHackage = liftScoutess $ \(buildSpec, buildSources) -> do
    let localHackage = tsLocalHackage (bsTargetSpec buildSpec)
        indexPath    = tmpDir </> "00-index.tar"
        sourceConfig = tsSourceConfig (bsTargetSpec buildSpec)
        tmpDir       = tsTmpDir (bsTargetSpec buildSpec)
        allSources   = targetSource buildSources : S.toList (depSources buildSources)
    mapM_ (flip addPackage localHackage) allSources
    generateIndexSelectively (Just (map srcVersionInfo allSources)) localHackage indexPath
    return $ LocalHackageIndex (indexPath <.> ".gz")

recordExceptions :: Scoutess (Set SourceException, Maybe (Set SourceException)) RecordedExceptions
recordExceptions = liftScoutess (return . uncurry RecordedExceptions)

-- | If if there was nothing to build, return a Left containing only the target's 'VersionInfo' and 'DependencyGraph' else
--   return a 'Right' containing the rest of the build report
build :: Scoutess (BuildSpec, LocalHackageIndex, SourceInfo, RecordedExceptions) BuildReport
build = liftScoutess $ \(buildSpec, localHackageIndex, targetSourceInfo, exceptions) -> do
    let targetSpec  = bsTargetSpec buildSpec
        sandboxDir  = tsPackageDB targetSpec
        installDir  = sandboxDir
        configFile  = tsTmpDir targetSpec </> "config"
        logLocation = tsTmpDir targetSpec </> "build.log"
    targetCabal <- fromMaybe (error "could not find target cabal file") <$> findCabalFile (srcPath targetSourceInfo)
    (ghcPkgExitCode, ghcPkgOut, ghcPkgErr) <-
        readProcessWithExitCode "ghc-pkg" ["init", sandboxDir] []
    when (ghcPkgExitCode /= ExitSuccess) $ do
        error ghcPkgErr -- temporary until a better sandboxing method is found
    writeFile configFile $ unlines
        [ "local-repo: " ++ hackageDir (tsLocalHackage (bsTargetSpec buildSpec))
        , "build-summary: " ++ logLocation
        , "remote-build-reporting: anonymous"
        ]
    let cabalArgs =
            ["--config-file=" ++ configFile
            ,"install"
            ,targetCabal
            ,"--package-db=clear"
            ,"--package-db=" ++ sandboxDir
            ,"--prefix=" ++ installDir
            ] ++ map T.unpack (tsCustomCabalArgs targetSpec)
    (exitCode, out, err) <- readProcessWithExitCode "cabal" cabalArgs []
    cabalLog <- readFile logLocation
    return BuildReport
        { brTarget          = srcVersionInfo targetSourceInfo
        , brDependencyGraph = bsDepGraph buildSpec
        , brExceptions      = exceptions
        , brPriorRun        = bsPriorRun buildSpec
        , brCabalResults    = Just (exitCode, T.pack out, T.pack err, T.pack cabalLog)
        }

notBuild :: Scoutess (BuildSpec, VersionInfo, RecordedExceptions) BuildReport
notBuild = liftScoutess $ \(buildSpec, targetVersion, exceptions) ->
    return BuildReport
        { brTarget          = targetVersion
        , brDependencyGraph = bsDepGraph buildSpec
        , brExceptions      = exceptions
        , brPriorRun        = bsPriorRun buildSpec
        , brCabalResults    = Nothing
        }