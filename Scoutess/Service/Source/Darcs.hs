{-# LANGUAGE OverloadedStrings #-}
-- | fetch (cabal) source using @darcs@
--
-- <http://darcs.net>
module Scoutess.Service.Source.Darcs where

import Control.Monad                         (when, unless)
import Control.Monad.Trans                   (MonadIO(..), liftIO)
import Data.Text                             (pack, unpack)
import qualified Data.Map as M
import Data.Set                              (singleton)
import Data.Version                          (showVersion)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Verbosity                (silent)
import System.Directory                      (createDirectoryIfMissing, doesDirectoryExist, removeDirectoryRecursive, renameDirectory)
import System.Exit                           (ExitCode(..))
import System.FilePath                       ((</>))
import System.Process                        (readProcessWithExitCode)

import Scoutess.Core
import Scoutess.Types

import Prelude hiding ((++))

-- | fetch source using @darcs@
fetchDarcs :: (MonadIO m) =>
              SourceConfig -- ^ 'SourceConfig'
           -> VersionInfo
           -> m (Either SourceException SourceInfo)
fetchDarcs sourceConfig versionInfo = do
    preFetched <- liftIO $ doesDirectoryExist destDir
    if not preFetched
      then do
        (exitCode, out, err) <- callDarcs (viSourceLocation versionInfo) destDir
        return $ case exitCode of
            ExitFailure _ -> Left . SourceErrorOther . pack $ err
            ExitSuccess   -> Right sourceInfo
      else return (Right sourceInfo)
    where
    destDir = srcCacheDir sourceConfig </> (viName versionInfo ++ "-" ++ showVersion (viVersion versionInfo))
    sourceInfo = SourceInfo destDir versionInfo

-- TODO: if the repo already exists, call darcs to update it
-- TODO: see the discussion at http://irclog.perlgeek.de/darcs/2012-04-10#i_5424281 for details of checking for updates
fetchVersionsDarcs :: (MonadIO m) =>
                      SourceConfig
                   -> SourceLocation
                   -> m (Either SourceException VersionSpec)
fetchVersionsDarcs sourceConfig sourceLocation = do
    let destParent = srcCacheDir sourceConfig
        destDir = destParent </> "tempDarcs"
    destParentExists <- liftIO $ doesDirectoryExist destParent
    unless destParentExists $ liftIO (createDirectoryIfMissing True destParent)
    destExists <- liftIO $ doesDirectoryExist destDir
    liftIO $ when destExists (removeDirectoryRecursive destDir) -- we shouldn't remove, but try to 'darcs fetch' instead!
    (exitCode, out, err) <- callDarcs sourceLocation destDir    -- so this command here shouldn't necessarily be 'get'
    result <- liftIO $ case exitCode of
        ExitFailure _ -> return . Left . SourceErrorOther . pack $ err
        ExitSuccess   -> do
            (Just cabalFile) <- liftIO $ findCabalFile destDir
            gpd <- liftIO $ readPackageDescription silent cabalFile
            let versionInfo = createVersionInfo sourceLocation gpd
                newDir      = srcCacheDir sourceConfig </> (viName versionInfo ++ "-" ++ showVersion (viVersion versionInfo))
            liftIO $ renameDirectory destDir newDir
            return . Right $ VersionSpec (singleton versionInfo) Nothing
    return result

callDarcs :: MonadIO m => SourceLocation -> FilePath -> m (ExitCode, String, String)
callDarcs (Darcs location maybeTag) destDir = liftIO $ readProcessWithExitCode "darcs" args []
    where args = ["get", unpack location, "--repo-name=" ++ destDir]
                  ++ maybe [] (\tag -> ["--tag =" ++ unpack tag]) maybeTag
callDarcs _ _ = error "callDarcs can only be called with the Darcs constructor for a SourceLocation."
