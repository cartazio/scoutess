{-# LANGUAGE Arrows, TupleSections, OverloadedStrings #-}
module Scoutess.DataFlow where

import Control.Applicative           ((<$>))
import Control.Arrow
import Control.Monad                 (when, forM_)
import Control.Monad.Error
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.Maybe                    (fromJust, fromMaybe, isNothing)
import Data.Monoid
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit                   (ExitCode(..))
import System.Process                (readProcessWithExitCode)
import System.Directory              (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, removeDirectoryRecursive, removeFile)
import System.FilePath               ((</>),(<.>))

import Scoutess.Core
import Scoutess.Service.LocalHackage.Core
import Scoutess.Service.Source.Fetch (fetchSrc, fetchSrcs, fetchVersion, fetchVersions)
import Scoutess.Types

import Distribution.Simple.Configure (getPersistBuildConfig)

import Prelude hiding ((++))

-- standard build
standard :: Scoutess SourceSpec SourceSpec    -- ^ sourceFilter
         -> Scoutess VersionSpec VersionSpec  -- ^ versionFilter
         -> Scoutess (SourceSpec, TargetSpec, Maybe PriorRun) BuildReport
standard sourceFilter versionFilter = proc (sourceSpec, targetSpec, mPriorRun) -> do
    initialiseSandbox                                   -< targetSpec
    (availableVersions, targetVersion)
                       <- fetchVersionSpec sourceFilter -< (targetSpec, sourceSpec)
    consideredVersions <- versionFilter                 -< availableVersions
    buildSpec          <- produceBuildSpec              -< (targetSpec, targetVersion, consideredVersions, mPriorRun)
    buildSources       <- getSources                    -< buildSpec
    index              <- updateLocalHackage            -< (buildSpec, buildSources)
    build                                               -< (buildSpec, targetSource buildSources, index)

-- | Clear the local hackage, ensure that the temp directory exists and is empty, and create an empty packageDB.
initialiseSandbox :: Scoutess TargetSpec ()
initialiseSandbox = withComponent "initialise" $ \targetSpec -> do
    liftIO $ clearLocalHackage (tsLocalHackage targetSpec)

    let emptyDirs = [tsTmpDir targetSpec]
    forM_ emptyDirs $ \filePath -> liftIO $ do
        dirExists <- doesDirectoryExist filePath
        when dirExists $ removeDirectoryRecursive filePath
        createDirectoryIfMissing True filePath

    let packageDB = tsPackageDB targetSpec
    packageDBExists <- liftIO $ doesDirectoryExist packageDB
    when packageDBExists $ liftIO (removeDirectoryRecursive packageDB)
    (exitCode, out, err) <- liftIO $ readProcessWithExitCode "ghc-pkg" ["init", packageDB] []
    report (T.unlines
        [ "Calling ghc-pkg init returned:"
        , "ExitCode:", T.pack (show exitCode)
        , "StdOut:"  , T.pack out
        , "StdErr:"  , T.pack err])
    componentFinish (exitCode == ExitSuccess) ()

-- | Fetch the available versions from the 'SourceSpec', take the target's 'VersionInfo'
--   and then return the filtered set in a 'VersionSpec'
fetchVersionSpec :: Scoutess SourceSpec SourceSpec -> Scoutess (TargetSpec, SourceSpec) (VersionSpec, VersionInfo)
fetchVersionSpec sourceFilter = withComponent "fetchVersionSpec" $ \(targetSpec, sourceSpec) -> do
    let name     = tsName targetSpec
        version  = tsVersion targetSpec
        location = tsLocation targetSpec
    filteredSources <- fst <$> (liftIO (runScoutess sourceFilter sourceSpec))
    when (isNothing filteredSources) $ do
        report "The sourceFilter failed"
        mzero
    let filteredSources'        = S.toList . ssLocations . fromJust $ filteredSources
    (exceptions, versionSpecs) <- liftIO $ fetchVersions (tsSourceConfig targetSpec) filteredSources'
    let combined = mconcat versionSpecs
        versionsErr = case exceptions of
            [] -> ""
            _  -> "Fetching the versions gave these exceptions: " ++ T.unlines (map sourceErrorMsg exceptions)
    report versionsErr

    -- eTargetVersions represents the locations we look for the target package in
    eTargetVersions <- if location `elem` filteredSources'
        then return (Right combined)
        else if location `elem` S.toList (ssLocations sourceSpec)
            then liftIO $ fetchVersion (tsSourceConfig targetSpec) location
            else return . Left $ strMsg "The target's location wasn't in the SourceSpec" -- TODO: replace this with a more helpful error message
    -- attempt to find the target package in eTargetVersions, dealing with errors
    either (reportFetchFail versionsErr) (reportOnFind versionsErr combined . findVersion name version location) eTargetVersions
    where
    -- can't fetch the versions from the location where the target is meant to be
    reportFetchFail versionsErr targetErr = do
        report $ T.pack ("Error when fetching the versions from the target's location: " ++ show targetErr)
        mzero
    -- could fetch the versions but can't find the target in them
    reportOnFind versionsErr _ Nothing = do
        report "The target wasn't found in the source locations."
        mzero
    -- could fetch the versions and found the target
    reportOnFind versionsErr combined (Just targetVersion) = componentPass (combined, targetVersion)

-- | Given the packages we can build from, ask cabal install to work out the dependency graph for us
--   by creating a package index for all fetchable versions (even though we don't actually have
--   any of the sources yet).
produceBuildSpec :: Scoutess (TargetSpec, VersionInfo, VersionSpec, Maybe PriorRun) BuildSpec
produceBuildSpec = withComponent "produceBuildSpec" $ \(targetSpec, targetInfo, versionSpec, mPriorRun) -> do
    let configFile  = tsTmpDir targetSpec </> "config"
        sandboxDir  = tsPackageDB targetSpec
        fakeRepo    = tsTmpDir targetSpec </> "fakeRepo"
    liftIO $ createPackageIndexWith const versionSpec fakeRepo -- XXX: 'const' will just pick packages with the highest SourceLocation
    liftIO $ writeFile configFile ("local-repo: " ++ fakeRepo)
    targetCabal <- liftIO $ writeCabal (tsTmpDir targetSpec) targetInfo
    let cabalArgs =
            ["--config-file=" ++ configFile
            ,"install"
            ,targetCabal
            ,"--package-db=clear"
            ,"--package-db=" ++ sandboxDir
            ,"--dry-run"]
    (exitCode, stdOut, stdErr) <- liftIO $ readProcessWithExitCode "cabal" cabalArgs []
    let deps        = parseDependencies stdOut `findIn` (vsVersions versionSpec)
        -- TODO: we still need to build if we need to produce docs (What about checking for up-to-date docs?)
        toBuild' pr = not (prTarget pr == targetInfo
                      && S.toList (prDependencies pr) == deps)
        toBuild     = maybe True toBuild' mPriorRun
        output      = T.pack $ unlines [ "Calling cabal install --dry-run returned:"
                                       , "ExitCode:", show exitCode, "StdOut:", stdOut, "StdErr:", stdErr]
        success     = exitCode == ExitSuccess
    report output
    guard success
    componentPass $ BuildSpec
        { bsTargetSpec   = targetSpec
        , bsTargetInfo   = targetInfo
        , bsDependencies = deps
        , bsToBuild      = toBuild }

-- | Fetch the sources for the target and its dependencies
getSources :: Scoutess BuildSpec BuildSources
getSources = withComponent "getSources" $ \buildSpec -> do
    let sourceConfig      = tsSourceConfig (bsTargetSpec buildSpec)
        depVersionInfos   = bsDependencies buildSpec
        targetVersionInfo = bsTargetInfo buildSpec
    eTargetSource <- fetchSrc sourceConfig targetVersionInfo
    when (isLeft eTargetSource) $ do
        report $ "Unable to fetch the target source with error:\n" ++ sourceErrorMsg (fromLeft eTargetSource)
        mzero
    let targetSource' = fromRight eTargetSource
    (exceptions, depSources') <- fetchSrcs sourceConfig depVersionInfos
    let depSourceReport = case exceptions of
          [] -> ""
          _  -> "fetching the sources gave these exceptions:\n" ++ T.unlines (map sourceErrorMsg exceptions)
    report depSourceReport
    componentPass $ BuildSources targetSource' depSources'
    where
    isLeft    (Left  _) = True
    isLeft    _         = False
    fromLeft  (Left  l) = l
    fromRight (Right r) = r

-- | Add all the sources to the local hackage index specified by the buildSpec
updateLocalHackage :: Scoutess (BuildSpec, BuildSources) LocalHackageIndex
updateLocalHackage = withComponent "updateLocalHackage" $ \(buildSpec, buildSources) -> do
    let localHackage = tsLocalHackage (bsTargetSpec buildSpec)
        indexPath    = tmpDir </> "00-index.tar"
        tmpDir       = tsTmpDir (bsTargetSpec buildSpec)
        allSources   = targetSource buildSources : depSources buildSources
    liftIO $ addPackages allSources localHackage
    index <- liftIO $ generateIndexSelectively (Just (map siVersionInfo allSources)) localHackage indexPath
    componentPass index

-- | Given a directive of what to build and the location of the target package,
--   call /cabal install/
build :: Scoutess (BuildSpec, SourceInfo, LocalHackageIndex) BuildReport
build = withComponent "build" $ \(buildSpec, targetSourceInfo, _) -> do
    let targetSpec            = bsTargetSpec buildSpec
        sandboxDir            = tsPackageDB targetSpec
        installDir            = sandboxDir
        configFile            = tsTmpDir targetSpec </> "config"
        logLocation           = tsTmpDir targetSpec </> "build.log"
        targetSourcePath      = siPath targetSourceInfo

    mTargetCabal <- liftIO $ findCabalFile targetSourcePath
    when (isNothing mTargetCabal) $ do
        report $ "Couldn't find the target's cabal file in: " ++ T.pack targetSourcePath
        mzero
    let targetCabal = fromJust mTargetCabal
    configExists <- liftIO $ doesFileExist configFile
    when configExists (liftIO $ removeFile configFile)

    liftIO . writeFile configFile $ unlines
        [ "local-repo: " ++ hackageDir (tsLocalHackage (bsTargetSpec buildSpec))
        , "build-summary: " ++ logLocation ]
    let cabalArgs =
            ["--config-file=" ++ configFile
            ,"install"
            ,targetCabal
            ,"--package-db=clear"
            ,"--package-db=" ++ sandboxDir
            ,"--prefix=" ++ installDir
            ] ++ map T.unpack (tsCustomCabalArgs targetSpec)
    (exitCode, out, err) <- liftIO $ readProcessWithExitCode "cabal" cabalArgs []
    let output = T.pack $ unlines [ "Calling cabal install returned:"
                                    , "ExitCode:", show exitCode, "StdOut:", out, "StdErr:", err]
    report output
    logExists <- liftIO $ doesFileExist logLocation
    cabalLog <- if logExists
        then lift . lift $ readFile logLocation
        else do
            report $ "The log file was not found (but should have been) at " ++ T.pack logLocation
            return ""
    guard (exitCode == ExitSuccess)
    localBuildInfo <- liftIO $ getPersistBuildConfig (targetSourcePath </> "dist")
    componentPass $ BuildReport buildSpec localBuildInfo

