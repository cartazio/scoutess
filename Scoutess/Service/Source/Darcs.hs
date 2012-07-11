{-# LANGUAGE OverloadedStrings #-}
-- | fetch (cabal) source using @darcs@
--
-- <http://darcs.net>
module Scoutess.Service.Source.Darcs where

import Control.Monad.Trans         (MonadIO(..), liftIO)
import Data.Text                   (Text)
import qualified Data.Text as Text (dropWhileEnd, unpack)
import Data.Set                    (Set)
import Distribution.Version        (Version)
import System.Exit                 (ExitCode(..))
import System.FilePath             ((</>))
import System.Process              (readProcessWithExitCode)

import Scoutess.Core

-- | fetch source using @darcs@
--   TODO: If we have already downloaded the source to get its version,
--   can we use that? How can we be sure it's the same source?
fetchDarcs :: (MonadIO m) =>
              SourceConfig -- ^ 'SourceConfig'
           -> VersionInfo
           -> m (Either SourceException SourceInfo)
fetchDarcs sourceConfig versionInfo = do
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
            return . Right $ SourceInfo {
                srcPath        = destDir
              , srcVersionInfo = versionInfo}
    where
    destDir = srcCacheDir sourceConfig </> viName versionInfo

fetchVersionsDarcs :: (MonadIO m) =>
                      SourceConfig
                   -> SourceLocation
                   -> m (Either SourceException (Set VersionInfo))
fetchVersionsDarcs = do
    -- 'get' the repo
    -- look for the cabal file
    -- create the VersionInfo (leaving the repo behind?)
    undefined