{-# LANGUAGE OverloadedStrings #-}

module Scoutess.Service.Source.Dir (fetchDir, fetchVersionsDir) where

import Control.Monad.Trans (MonadIO, liftIO)
import Data.Set (singleton)
import Data.Text (pack)
import Data.Version (showVersion)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Simple.Utils (installDirectoryContents)
import Distribution.Verbosity (silent)
import System.FilePath ((</>))

import Scoutess.Core
import Scoutess.Types

import Prelude hiding ((++))

fetchDir :: MonadIO m =>
            SourceConfig
         -> VersionInfo
         -> m (Either SourceException SourceInfo)
fetchDir sourceConfig versionInfo = do
    let Dir oldPath  = viSourceLocation versionInfo
        nameVersion  = viName versionInfo ++ "-" ++ showVersion (viVersion versionInfo)
        newPath      = srcCacheDir sourceConfig </> nameVersion
    liftIO $ installDirectoryContents silent oldPath newPath
    return . Right $ SourceInfo newPath versionInfo

fetchVersionsDir :: MonadIO m =>
                    SourceLocation
                 -> m (Either SourceException VersionSpec)
fetchVersionsDir sourceLocation = do
    let Dir filePath = sourceLocation
    mCabalFile <- liftIO $ findCabalFile filePath
    case mCabalFile of
        Nothing -> do
            return . Left . SourceErrorOther $ "Could not find .cabal file in " ++ pack filePath
        Just cabalFile -> do
            gpd <- liftIO $ readPackageDescription silent cabalFile
            let versionInfo = createVersionInfo sourceLocation gpd
            return . Right $ VersionSpec (singleton versionInfo) Nothing
