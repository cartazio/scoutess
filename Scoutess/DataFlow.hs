{-# LANGUAGE Arrows #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Scoutess.DataFlow where

import Control.Applicative           ((<$>))
import Control.Arrow
import Control.Monad                 (when, forM_)
import Data.Maybe                    (fromMaybe)
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as T
import System.Exit                   (ExitCode(..))
import System.Process                (readProcessWithExitCode)
import System.Directory              (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, removeDirectoryRecursive, removeFile)
import System.FilePath               ((</>),(<.>))

import Scoutess.Core
import Scoutess.Service.LocalHackage.Core
import Scoutess.Service.Source.Fetch (fetchSrc, fetchSrcs, fetchVersion, fetchVersions)

-- standard build
standard :: Scoutess SourceSpec SourceSpec    -- ^ sourceFilter
         -> Scoutess VersionSpec VersionSpec  -- ^ versionFilter
         -> Scoutess (SourceSpec, TargetSpec, Maybe PriorRun) BuildReport
standard sourceFilter versionFilter = proc (sourceSpec, targetSpec, mPriorRun) -> do
    initialiseSandbox -< targetSpec
    (availableVersions, targetVersion)
                       <- fetchVersionSpec sourceFilter -< (targetSpec, sourceSpec)
    consideredVersions <- versionFilter                 -< availableVersions
    buildSpec          <- produceBuildSpec              -< (targetSpec, targetVersion, consideredVersions, mPriorRun)
    if bsToBuild buildSpec
      then do
        buildSources   <- getSources                    -< buildSpec
        updateLocalHackage                              -< (buildSpec, buildSources)
        buildReport    <- build                         -< (buildSpec, buildSources)
        returnA -< buildReport
      else do
        notBuild                                      -< buildSpec

-- ensure that the temp directory and localHackage have been cleared and
-- that the package-db directory doesn't exist
initialiseSandbox :: Scoutess TargetSpec ()
initialiseSandbox = liftScoutess $ \targetSpec -> do
    clearLocalHackage (tsLocalHackage targetSpec)
    forM_ [tsTmpDir targetSpec] $ \filePath -> do
        dirExists <- doesDirectoryExist filePath
        when dirExists (removeDirectoryRecursive filePath)
        createDirectoryIfMissing True filePath
    pkgDirExists <- doesDirectoryExist (tsPackageDB targetSpec)
    when pkgDirExists (removeDirectoryRecursive (tsPackageDB targetSpec))
    (ghcPkgExitCode, ghcPkgOut, ghcPkgErr) <- readProcessWithExitCode "ghc-pkg" ["init", tsPackageDB targetSpec] []
    when (ghcPkgExitCode /= ExitSuccess) $ error ghcPkgErr -- temporary until a better sandboxing method is found

-- | fetch the 'VersionSpec' for the dependencies and the 'VersionInfo' for the target package.
--   Note that the dependencies are fetched from the filtered 'SourceSpec' while the target is
--   from the unfiltered 'SourceSpec'.
fetchVersionSpec :: Scoutess SourceSpec SourceSpec -> Scoutess (TargetSpec, SourceSpec) (VersionSpec, VersionInfo)
fetchVersionSpec sourceFilter = liftScoutess $ \(targetSpec, sourceSpec) -> do
    let name     = tsName targetSpec
        version  = tsVersion targetSpec
        location = tsLocation targetSpec
    filteredSources <- S.toList . ssLocations <$> runScoutess sourceFilter sourceSpec
    (exceptions, versionSpecs) <- fetchVersions (tsSourceConfig targetSpec) filteredSources
    let combined = mconcat versionSpecs
    mTargetVersion <- if location `elem` filteredSources
        then return $ findVersion name version location combined
        else either (error . T.unpack . sourceErrorMsg) (findVersion name version location) <$> fetchVersion (tsSourceConfig targetSpec) location
    let targetVersion = fromMaybe (error "could not find the target in the sources") mTargetVersion
    return (combined, targetVersion)

produceBuildSpec :: Scoutess (TargetSpec, VersionInfo, VersionSpec, Maybe PriorRun) BuildSpec
produceBuildSpec = liftScoutess $ \(targetSpec, targetInfo, versionSpec, mPriorRun) -> do
    let configFile  = tsTmpDir targetSpec </> "config"
        sandboxDir  = tsPackageDB targetSpec
        fakeRepo    = tsTmpDir targetSpec </> "fakeRepo"
    createPackageIndexWith const versionSpec fakeRepo -- XXX: 'const' will just pick packages with the highest SourceLocation
    writeFile configFile $ "local-repo: " ++ fakeRepo
    targetCabal <- writeCabal (tsTmpDir targetSpec) targetInfo
    let cabalArgs =
            ["--config-file=" ++ configFile
            ,"install"
            ,targetCabal
            ,"--package-db=clear"
            ,"--package-db=" ++ sandboxDir
            ,"--dry-run"]
    (exitCode, stdOut, stdErr) <- readProcessWithExitCode "cabal" cabalArgs []
    let deps = parseDependencies stdOut `findIn` (vsVersions versionSpec)
        toBuild pr = not (prTarget pr == targetInfo
                      && S.toList (prDependencies pr) == deps)
    return BuildSpec
        { bsTargetSpec   = targetSpec
        , bsTargetInfo   = targetInfo
        , bsDependencies = deps
        , bsPriorRun     = mPriorRun
        , bsToBuild      = maybe True toBuild mPriorRun
        }

-- | Fetch the sources for the target and its dependencies
getSources :: Scoutess BuildSpec BuildSources
getSources = liftScoutess $ \buildSpec -> do
    let sourceConfig      = tsSourceConfig (bsTargetSpec buildSpec)
        depVersionInfos   = bsDependencies buildSpec
        targetVersionInfo = bsTargetInfo buildSpec
    targetSource' <- either (error . T.unpack . sourceErrorMsg) id <$> fetchSrc sourceConfig targetVersionInfo
    (exceptions, depSources') <- fetchSrcs sourceConfig depVersionInfos
    return $ BuildSources targetSource' depSources'

-- | Add all the sources to the local hackage index specified by the buildSpec
updateLocalHackage :: Scoutess (BuildSpec, BuildSources) ()
updateLocalHackage = liftScoutess $ \(buildSpec, buildSources) -> do
    let localHackage = tsLocalHackage (bsTargetSpec buildSpec)
        indexPath    = tmpDir </> "00-index.tar"
        tmpDir       = tsTmpDir (bsTargetSpec buildSpec)
        allSources   = targetSource buildSources : depSources buildSources
    addPackages allSources localHackage
    generateIndexSelectively (Just (map siVersionInfo allSources)) localHackage indexPath

-- | If if there was nothing to build, return a Left containing only the target's 'VersionInfo' and 'DependencyGraph' else
--   return a 'Right' containing the rest of the build report
build :: Scoutess (BuildSpec, BuildSources) BuildReport
build = liftScoutess $ \(buildSpec, buildSources) -> do
    let targetSpec  = bsTargetSpec buildSpec
        sandboxDir  = tsPackageDB targetSpec
        installDir  = sandboxDir
        configFile  = tsTmpDir targetSpec </> "config"
        logLocation = tsTmpDir targetSpec </> "build.log"
        targetSourceInfo = targetSource buildSources
    targetCabal <- fromMaybe (error "could not find target cabal file") <$> findCabalFile (siPath targetSourceInfo)
    configExists <- doesFileExist configFile
    when configExists (removeFile configFile)
    writeFile configFile $ unlines
        [ "local-repo: " ++ hackageDir (tsLocalHackage (bsTargetSpec buildSpec))
        , "build-summary: " ++ logLocation ]
    let cabalArgs =
            ["--config-file=" ++ configFile
            ,"install"
            ,targetCabal
            ,"--package-db=clear"
            ,"--package-db=" ++ sandboxDir
            ,"--prefix=" ++ installDir
            ,"--enable-documentation"
            ,"--docdir=" ++ sandboxDir </> "docs"
            ] ++ map T.unpack (tsCustomCabalArgs targetSpec)
    (exitCode, out, err) <- readProcessWithExitCode "cabal" cabalArgs []
    logExists <- doesFileExist logLocation
    cabalLog <- if logExists
        then readFile logLocation
        else putStrLn ("the log file didn't exist at: " ++ logLocation) >> return ""
    return BuildReport
        { brBuildSpec       = buildSpec
        , brCabalResults    = Just (exitCode, T.pack out, T.pack err, T.pack cabalLog)
        , brBuildSources    = Just buildSources
        }

notBuild :: Scoutess BuildSpec BuildReport
notBuild = liftScoutess $ \buildSpec ->
    return BuildReport
        { brBuildSpec       = buildSpec
        , brCabalResults    = Nothing
        , brBuildSources    = Nothing
        }
