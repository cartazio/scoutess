{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
-- | fetch packages from hackage using @Network.HTTP@

module Scoutess.Service.Source.Hackage (fetchHackage, fetchVersionsHackage) where

import Control.Monad.Trans                   (MonadIO(..), liftIO)
import Data.Text                             (Text)
import qualified Data.Text as Text
import Data.Version                          (showVersion)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Data.Set                              (Set)
import qualified Data.Set as S
import Distribution.Verbosity                (silent)
import System.FilePath                       ((</>))
import System.Directory                      (createDirectoryIfMissing, renameDirectory, doesDirectoryExist, removeDirectoryRecursive, removeFile)

import Scoutess.Core
import Scoutess.Utils.Archives
import Scoutess.Utils.HTTP

baseUrl :: String
baseUrl = "http://hackage.haskell.org/packages/archive/"

packageUrl :: Text -> Text -> String
packageUrl pkgName pkgVersion = baseUrl ++ Text.unpack pkgName ++ "/" ++ Text.unpack pkgVersion ++ "/" ++ Text.unpack pkgName ++ "-" ++ Text.unpack pkgVersion ++ ".tar.gz"

-- | Fetch a given package from Hackage and return its SourceInfo
fetchHackage :: (MonadIO m) =>
                SourceConfig
             -> VersionInfo
             -> m (Either SourceException SourceInfo)
fetchHackage sourceConfig versionInfo = do
  let pkgName    = Text.pack . viName $ versionInfo
      pkgVersion = Text.pack . showVersion . viVersion $ versionInfo
      pkgUrl     = packageUrl pkgName pkgVersion
      localPath  = srcCacheDir sourceConfig </> (Text.unpack pkgName ++ "-" ++ Text.unpack pkgVersion ++ ".tar.gz")
  dledPath <- liftIO $ downloadFile pkgUrl localPath
  case dledPath of
    Just _ -> do
      let destDir = srcCacheDir sourceConfig </> Text.unpack pkgName
      liftIO $ createDirectoryIfMissing True destDir
      liftIO $ extractArchive localPath destDir
      liftIO $ renameDirectory (destDir </> Text.unpack pkgName ++ "-" ++ Text.unpack pkgVersion) (destDir </> Text.unpack pkgVersion)
      liftIO $ removeFile localPath
      return . Right $ SourceInfo { srcPath        = destDir </> Text.unpack pkgVersion
                                  , srcVersionInfo = versionInfo}
    Nothing -> return . Left $ SourceErrorOther "Couldn't download the package archive. Please check that your connection and the hackage.haskell.org server are up."

-- | Creates a VersionInfo for each package on Hackage and returns them in a Set
fetchVersionsHackage :: (MonadIO m) =>
                  SourceConfig -- ^ 'SourceConfig' defining where the 00-index is to be stored
               -> m (Either SourceException (Set VersionInfo))
fetchVersionsHackage sourceConfig = do
  liftIO $ createDirectoryIfMissing True (srcCacheDir sourceConfig)
  pkgIndex' <- liftIO $ updateFile "http://hackage.haskell.org/packages/archive/00-index.tar.gz" (srcCacheDir sourceConfig </> "00-index.tar.gz")
  case pkgIndex' of
    Nothing       -> return . Left . SourceErrorOther $ "Couldn't download the package index."
    Just pkgIndex -> do
      liftIO $ extractArchive pkgIndex (tmpDir </> "unpack")
      cabals              <- liftIO $ findCabalFiles tmpDir
      packageDescriptions <- liftIO $ mapM (readPackageDescription silent) cabals
      liftIO $ removeDirectoryRecursive (tmpDir </> "unpack")
      return . Right . S.fromList . map (createVersionInfo Hackage) $ packageDescriptions
  where tmpDir = srcCacheDir sourceConfig </> "tmp"
