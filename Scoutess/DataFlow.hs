{-# LANGUAGE Arrows, TupleSections, OverloadedStrings #-}
module Scoutess.DataFlow where

import Control.Applicative           ((<$>))
import Control.Arrow
import Control.Monad                 (when, forM_)
import Control.Monad.Error
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.Either                   (partitionEithers)
import Data.Maybe                    (catMaybes, fromJust, fromMaybe, isNothing)
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

-- standard build
standard :: Scoutess SourceSpec SourceSpec    -- ^ sourceFilter
         -> Scoutess VersionSpec VersionSpec  -- ^ versionFilter
         -> Scoutess (SourceSpec, TargetSpec, Maybe PriorRun) BuildReport
standard sourceFilter versionFilter =
    proc (sourceSpec, targetSpec, mPriorRun) -> do
    let locations = mkLocations (tsTmpDir targetSpec) (tsGhcPath targetSpec)
    initializeSandbox                                   -< (targetSpec, locations)
    availableVersions  <- fetchVersionSpec sourceFilter -< (sourceSpec, locations)
    consideredVersions <- versionFilter                 -< availableVersions
    buildSpec          <- produceBuildSpec              -< (targetSpec, consideredVersions, mPriorRun, locations)
    buildSources       <- getSources                    -< (buildSpec, locations)
    index              <- updateLocalHackage            -< (buildSpec, buildSources, locations)
    buildReport        <- build                         -< (buildSpec, targetSource buildSources, index, locations)
--    ()                 <- haddock                       -< (buildSpec, targetSource buildSources, index, locations)
    returnA            -< buildReport

-- | Clear the local hackage, ensure that the temp directory exists and is empty, and create an empty packageDB.
initializeSandbox :: Scoutess (TargetSpec, Locations) ()
initializeSandbox = withComponent "initialize" $ \(targetSpec, locations) -> do
    liftIO $ clearLocalHackage (lLocalHackage locations)

    let emptyDirs = [] -- [lTmpDir locations]
    forM_ emptyDirs $ \filePath -> liftIO $ do
        dirExists <- doesDirectoryExist filePath
        when dirExists $ removeDirectoryRecursive filePath
        createDirectoryIfMissing True filePath

{-
    let packageDB = lPackageDB locations
    packageDBExists <- liftIO $ doesDirectoryExist packageDB
    when packageDBExists $ liftIO (removeDirectoryRecursive packageDB)
    (exitCode, out, err) <- liftIO $ readProcessWithExitCode "ghc-pkg" ["init", packageDB] []
    report (T.unlines
        [ "Calling ghc-pkg init returned:"
        , "ExitCode:", T.pack (show exitCode)
        , "StdOut:"  , T.pack out
        , "StdErr:"  , T.pack err])
-}
    let sandboxDir = lSandboxDir locations
    (exitCode, out, err) <- liftIO $ readProcessWithExitCode "cabal" ["sandbox", "init", "--sandbox", sandboxDir] []
    report (T.unlines
        [ "Calling ghc-pkg init returned:"
        , "ExitCode:", T.pack (show exitCode)
        , "StdOut:"  , T.pack out
        , "StdErr:"  , T.pack err
        ])
    componentFinish (exitCode == ExitSuccess) ()
--    when (exitCode /= ExitSuccess) componentFatal



-- | Fetch the available versions from the 'SourceSpec', take the target's 'VersionInfo'
--   and then return the filtered set in a 'VersionSpec'
fetchVersionSpec :: Scoutess SourceSpec SourceSpec -> Scoutess (SourceSpec, Locations) VersionSpec
fetchVersionSpec sourceFilter = withComponent "fetchVersionSpec" $ \(sourceSpec, locations) -> do
    filteredSources <- fst <$> (liftIO (runScoutess sourceFilter sourceSpec))
    when (isNothing filteredSources) $ do
        report "The sourceFilter failed"
        mzero
    let filteredSources'        = S.toList . ssLocations . fromJust $ filteredSources
    (exceptions, versionSpecs) <- liftIO $ fetchVersions (lSourceConfig locations) filteredSources'
    let combined = mconcat versionSpecs
        versionsErr = case exceptions of
            [] -> ""
            _  -> "Fetching the versions gave these exceptions: " <> T.unlines (map sourceErrorMsg exceptions)
    report versionsErr
    return (True, combined)

findTargets :: VersionSpec -> TargetSpec -> [Maybe VersionInfo]
findTargets versionSpec targetSpec =
    map findTarget (S.toList $ tsTargets targetSpec)
    where
      findTarget :: Target -> Maybe VersionInfo
      findTarget target =
          findVersion (tName target) (tVersion target) (tLocation target) versionSpec

-- | Given the packages we can build from, ask cabal install to work out the dependency graph for us
--   by creating a package index for all fetchable versions (even though we don't actually have
--   any of the sources yet).
produceBuildSpec :: Scoutess (TargetSpec, VersionSpec, Maybe PriorRun, Locations) BuildSpec
produceBuildSpec = withComponent "produceBuildSpec" $ \(targetSpec, versionSpec, mPriorRun, locations) -> do
    let configFile  = lTmpDir locations </> "config"
        sandboxDir  = lPackageDB locations
        fakeRepo    = lTmpDir locations </> "fakeRepo"
        ghcPath     = lGhcPath locations
    liftIO $ createPackageIndexWith const versionSpec fakeRepo -- XXX: 'const' will just pick packages with the highest SourceLocation
    liftIO $ writeFile configFile ("local-repo: " <> fakeRepo)
    let versionInfos = catMaybes $ findTargets versionSpec targetSpec -- XXX: shouldn't just discard failures
    targetCabals <- liftIO $ mapM (writeCabal (lTmpDir locations)) versionInfos


    let cabalArgs =
            [-- "--config-file=" <> configFile
              "-v"
            , "--package-db=clear"
            , "--with-compiler=" <> ghcPath
--            ,"--package-db=global" -- THIS IS TEMPORARY! See <http://hackage.haskell.org/trac/ghc/ticket/5977>
--            ,"--package-db=" <> sandboxDir
            , "--dry-run"
            , "install"] ++
            targetCabals
    (exitCode, stdOut, stdErr) <- liftIO $ readProcessWithExitCode "cabal" cabalArgs []
    let deps        = parseDependencies stdOut `findIn` (vsVersions versionSpec)
        -- TODO: we still need to build if we need to produce docs (What about checking for up-to-date docs?)
{-
        toBuild' pr = not (prTarget pr == targetInfo
                      && S.toList (prDependencies pr) == deps)
        toBuild     = maybe True toBuild' mPriorRun
-}
        output      = T.pack $ unlines [ "Calling cabal install --dry-run returned:"
                                       , "ExitCode:", show exitCode, "StdOut:", stdOut, "StdErr:", stdErr]
        success     = exitCode == ExitSuccess
    report output
    guard success
    componentPass $ BuildSpec
        { bsTargetSpec   = targetSpec
        , bsTargetInfo   = versionInfos
        , bsDependencies = deps
        , bsToBuild      = True } -- FIXME: calculate if we really need to run

-- | Fetch the sources for the target and its dependencies
getSources :: Scoutess (BuildSpec, Locations) BuildSources
getSources = withComponent "getSources" $ \(buildSpec, locations) -> do
    let sourceConfig       = lSourceConfig locations
        depVersionInfos    = bsDependencies buildSpec
        targetVersionInfos = bsTargetInfo buildSpec
{-
    eTargetSources <- mapM (fetchSrc sourceConfig) targetVersionInfos
    when (isLeft eTargetSource) $ do
        report $ "Unable to fetch the target source with error:\n" <> sourceErrorMsg (fromLeft eTargetSource)
        mzero
    let targetSource' = fromRight eTargetSource
-}
    (exceptions', targetSources) <- fetchSrcs sourceConfig targetVersionInfos
    (exceptions, depSources') <- fetchSrcs sourceConfig depVersionInfos
    let depSourceReport = case exceptions of
          [] -> ""
          _  -> "fetching the sources gave these exceptions:\n" <> T.unlines (map sourceErrorMsg exceptions)
    report depSourceReport
    componentPass $ BuildSources targetSources depSources'
    where
    isLeft    (Left  _) = True
    isLeft    _         = False
    fromLeft  (Left  l) = l
    fromRight (Right r) = r


-- | Add all the sources to the local hackage index specified by the buildSpec
updateLocalHackage :: Scoutess (BuildSpec, BuildSources, Locations) LocalHackageIndex
updateLocalHackage = withComponent "updateLocalHackage" $ \(buildSpec, buildSources, locations) -> do
    let localHackage = lLocalHackage locations
        tmpDir       = lTmpDir locations
        indexPath    = tmpDir </> "00-index.tar"
        allSources   = targetSource buildSources ++ depSources buildSources
    liftIO $ addPackages allSources localHackage
    index <- liftIO $ generateIndexSelectively (Just (map siVersionInfo allSources)) localHackage indexPath
    componentPass index

-- | Given a directive of what to build and the location of the target package,
--   call /cabal install/
build :: Scoutess (BuildSpec, [SourceInfo], LocalHackageIndex, Locations) BuildReport
build = withComponent "build" $ \(buildSpec, targetSourceInfos, _, locations) -> do
    let targetSpec            = bsTargetSpec buildSpec
        sandboxDir            = lPackageDB locations
        installDir            = sandboxDir
        configFile            = lTmpDir locations </> "config"
--        logLocation           = lTmpDir locations </> "cabal-sandbox" </> "logs" </> "build.log"
        targetSourcePaths     = map siPath targetSourceInfos

    mTargetCabals <- liftIO $ mapM findCabalFile targetSourcePaths
    when (any isNothing mTargetCabals) $ do
        report $ "Couldn't find the target's cabal file in: " -- FIXME <> T.pack targetSourcePath
        mzero

    let targetCabals = catMaybes mTargetCabals
    configExists <- liftIO $ doesFileExist configFile
    when configExists (liftIO $ removeFile configFile)

    liftIO . writeFile configFile $ unlines
        [ "local-repo: " <> hackageDir (lLocalHackage locations)
        ] -- , "build-summary: " <> logLocation ]
    let cabalArgs =
            [-- "--config-file=" <> configFile
--            ,"--package-db=clear"
--            ,"--package-db=global" -- THIS IS TEMPORARY TOO: <http://hackage.haskell.org/trac/ghc/ticket/5977>
--            ,"--package-db=" <> sandboxDir
--            ,"--prefix=" <> installDir
              "-v"
            , "--package-db=clear"
            , "--with-compiler=" <> (lGhcPath locations)
            ,"install"
            ] ++ targetCabals
-- <> map T.unpack (tsCustomCabalArgs targetSpec)
    (exitCode, out, err) <- liftIO $ readProcessWithExitCode "cabal" cabalArgs []
    let output = T.pack $ unlines [ "Calling cabal install returned:"
                                    , "ExitCode:", show exitCode, "StdOut:", out, "StdErr:", err]
    report output
--    logExists <- liftIO $ doesFileExist logLocation
    -- TODO: do we need the log if we have the LocalBuildInfo?
{-
    cabalLog <- if logExists
        then lift . lift $ readFile logLocation
        else do
            report $ "The log file was not found (but should have been) at " <> T.pack logLocation
            return ""
-}
    guard (exitCode == ExitSuccess)
    -- FIXME: this is failing because the sandbox puts things in a different path
--    localBuildInfos <- liftIO $ mapM (\targetSourcePath -> getPersistBuildConfig (targetSourcePath </> "dist")) targetSourcePaths
    componentPass $ BuildReport buildSpec [] -- localBuildInfos

-- | Given a directive of what to build and the location of the target package,
--   call /cabal install/
haddock :: Scoutess (BuildSpec, [SourceInfo], LocalHackageIndex, Locations) ()
haddock = withComponent "haddock-standalone" $ \(buildSpec, targetSourceInfos, _, locations) -> do
    let targetSpec            = bsTargetSpec buildSpec
        sandboxDir            = lPackageDB locations
        installDir            = sandboxDir
        configFile            = tsTmpDir targetSpec </> "config"
--        logLocation           = tsTmpDir targetSpec </> "cabal-sandbox" </> "logs" </> "build.log"
        targetSourcePaths     = map siPath targetSourceInfos
{-
    mTargetCabals <- liftIO $ mapM findCabalFile targetSourcePaths
    when (isNothing mTargetCabal) $ do
        report $ "Couldn't find the target's cabal file in: " <> T.pack targetSourcePath
        mzero
    let targetCabal = fromJust mTargetCabal
    configExists <- liftIO $ doesFileExist configFile
    when configExists (liftIO $ removeFile configFile)
-}
    liftIO . writeFile configFile $ unlines
        [ "local-repo: " <> hackageDir (lLocalHackage locations)
        ]
--        , "build-summary: " <> logLocation ]

    let args = [ "--package-db=" <> "_scoutess-build/cabal-sandbox/x86_64-linux-ghc-7.6.3-packages.conf.d"
               , "-o", "doc-html"
               ] ++ targetSourcePaths
{-
            ["--config-file=" <> configFile
            ,"install"
            ,targetCabal
            ,"--package-db=clear"
            ,"--package-db=global" -- THIS IS TEMPORARY TOO: <http://hackage.haskell.org/trac/ghc/ticket/5977>
            ,"--package-db=" <> sandboxDir
            ,"--prefix=" <> installDir
            ] <> map T.unpack (tsCustomCabalArgs targetSpec)

-}
    report $ mconcat $ map T.pack ("standalone-standalone":args)
    (exitCode, out, err) <- liftIO $ readProcessWithExitCode "standalone-haddock" args []
    let output = T.pack $ unlines [ "Calling standalone-haddock returned:"
                                    , "ExitCode:", show exitCode, "StdOut:", out, "StdErr:", err]
    report output
{-
    logExists <- liftIO $ doesFileExist logLocation
    -- TODO: do we need the log if we have the LocalBuildInfo?
    cabalLog <- if logExists
        then lift . lift $ readFile logLocation
        else do
            report $ "The log file was not found (but should have been) at " <> T.pack logLocation
            return ""
    guard (exitCode == ExitSuccess)
-}
--    localBuildInfos <- liftIO $ mapM (\targetSourcePath -> getPersistBuildConfig (targetSourcePath </> "dist")) targetSourcePaths
    componentPass ()



