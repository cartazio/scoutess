{-# LANGUAGE OverloadedStrings #-}
-- | fetch (cabal) source using @darcs@
--
-- <http://darcs.net>
module Scoutess.Service.Source.Darcs where

import Control.Monad                         (when)
import Control.Monad.Trans                   (MonadIO(..), liftIO)
import Data.Text                             (Text)
import qualified Data.Text as Text           (dropWhileEnd, unpack)
import Data.Set                              (Set, singleton)
import Data.Version                          (showVersion)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Verbosity                (silent)
import System.Directory                      (doesDirectoryExist, removeDirectoryRecursive, renameDirectory)
import System.Exit                           (ExitCode(..))
import System.FilePath                       ((</>))
import System.Process                        (readProcessWithExitCode)

import Scoutess.Core

-- XXX: not tested yet

-- | fetch source using @darcs@
--   TODO: If we have already downloaded the source to get its version,
--   can we use that? How can we be sure it's the same source?
fetchDarcs :: (MonadIO m) =>
              SourceConfig -- ^ 'SourceConfig'
           -> VersionInfo
           -> m (Either SourceException SourceInfo)
fetchDarcs sourceConfig versionInfo = do
    preFetched <- liftIO $ doesDirectoryExist destDir
    if not preFetched
      then do
        let (Darcs location maybeTag) = viSourceLocation versionInfo
            args = ["get", "\"" ++ Text.unpack location ++ "\""
                  ,"--repo-name=\"" ++ destDir ++ "\""]
                     ++ case maybeTag of
                          Nothing  -> []
                          Just tag -> ["--tag =\"" ++ Text.unpack tag ++ "\""]
        -- escape the command line arguments?
        (exitCode, out, err) <- liftIO $ readProcessWithExitCode "darcs" args []
        liftIO $ case exitCode of
            ExitFailure _ -> return . Left $ SourceErrorOther "Couldn't download the package. Please check your connection and the repo location."
            ExitSuccess   -> do
            return (Right sourceInfo)
      else return (Right sourceInfo)
    where
    destDir = srcCacheDir sourceConfig </> (viName versionInfo ++ "-" ++ showVersion (viVersion versionInfo))
    sourceInfo = SourceInfo destDir versionInfo

fetchVersionsDarcs :: (MonadIO m) =>
                      SourceConfig
                   -> SourceLocation
                   -> m (Either SourceException (Set VersionInfo))
fetchVersionsDarcs sourceConfig sourceLocation = do
    let destDir = srcCacheDir sourceConfig </> "tempDarcs"
        Darcs location maybeTag = sourceLocation
        args = ["get", "\"" ++ Text.unpack location ++ "\""
               ,"--repo-name=\"" ++ destDir ++ "\""]
        args' = args ++ case maybeTag of
            Nothing  -> []
            Just tag -> ["--tag =\"" ++ Text.unpack tag ++ "\""]
    (exitCode, out, err) <- liftIO $ readProcessWithExitCode "darcs" args' []
    result <- liftIO $ case exitCode of
        ExitFailure _ -> return . Left $ SourceErrorOther "Couldn't download the package. Please check your connection and the repo location."
        ExitSuccess   -> do
            (Just cabalFile) <- liftIO $ findCabalFile destDir
            gpd <- liftIO $ readPackageDescription silent cabalFile
            let versionInfo = createVersionInfo sourceLocation gpd
                newDir      = srcCacheDir sourceConfig </> (viName versionInfo ++ "-" ++ showVersion (viVersion versionInfo))
            liftIO $ renameDirectory destDir newDir
            return . Right . singleton $ versionInfo
    destExists <- liftIO $ doesDirectoryExist destDir
    liftIO $ when destExists (removeDirectoryRecursive destDir)
    return result