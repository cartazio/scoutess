{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
-- | fetch packages from hackage using @Network.HTTP@

module Scoutess.Service.Source.Hackage (fetchHackage, fetchVersionsHackage) where

import Control.Monad                         (liftM, forM)
import Control.Monad.Trans                   (MonadIO(..), liftIO)
import Data.Text                             (Text)
import qualified Data.Text as Text
import Data.Version                          (showVersion)
import Distribution.PackageDescription.Parse (readPackageDescription)
import qualified Data.Set as S
import Distribution.Verbosity                (silent)
import System.FilePath                       ((</>))
import System.Directory                      (createDirectoryIfMissing, renameDirectory, doesDirectoryExist, doesFileExist,)

import Scoutess.Core
import Scoutess.Types
import Scoutess.Utils.Archives
import Scoutess.Utils.HTTP

import Prelude hiding ((++))

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
-- TODO: clean up
    let pkgName      = Text.pack . viName $ versionInfo
        pkgVersion   = Text.pack . showVersion . viVersion $ versionInfo
        pkgUrl       = packageUrl pkgName pkgVersion
        archiveLoc   = srcCacheDir sourceConfig </> (Text.unpack pkgName ++ "-" ++ Text.unpack pkgVersion ++ ".tar.gz")
        destDir      = srcCacheDir sourceConfig </> Text.unpack pkgName
        fullCacheDir = destDir </> Text.unpack pkgVersion
    cacheExists <- liftIO $ doesDirectoryExist fullCacheDir
    if cacheExists
      then return . Right $ SourceInfo fullCacheDir versionInfo
      else liftIO $ downloadFile pkgUrl archiveLoc >>= \mDledPath -> case mDledPath of
        Just dledPath -> liftIO $ do
            createDirectoryIfMissing True destDir
            extractArchive dledPath destDir
            renameDirectory (destDir </> Text.unpack pkgName ++ "-" ++ Text.unpack pkgVersion) fullCacheDir
            return . Right $ SourceInfo fullCacheDir versionInfo
        Nothing -> return . Left $ SourceErrorOther "Couldn't download the package archive. Please check that your connection and the hackage.haskell.org server are up."

-- | Creates a VersionInfo for each package on Hackage and returns them in a VersionSpec
fetchVersionsHackage :: (MonadIO m) =>
                  SourceConfig -- ^ 'SourceConfig' defining where the 00-index is to be stored
               -> m (Either SourceException VersionSpec)
fetchVersionsHackage sourceConfig = do
  liftIO $ createDirectoryIfMissing True (srcCacheDir sourceConfig)
  pkgIndex' <- liftIO $ updateFile "http://hackage.haskell.org/packages/archive/00-index.tar.gz" (srcCacheDir sourceConfig </> "00-index.tar.gz")
  case pkgIndex' of
    Nothing       -> return . Left . SourceErrorOther $ "Couldn't download the package index."
    Just pkgIndex -> do
      liftIO $ extractArchive pkgIndex (tmpDir </> "unpack")
      let prefFile = tmpDir </> "unpack" </> "preferred-versions"
      prefExists <- liftIO $ doesFileExist prefFile
      pref <- if prefExists then (Just . Text.pack) `liftM` liftIO (readFile prefFile) else return Nothing
      cabals              <- {-# SCC "hi" #-} liftIO $ findCabalFiles tmpDir
      versionInfos        <- {-# SCC "hi2" #-} liftIO $ forM cabals $ \cabalFile -> do
          packageDescription <- readPackageDescription silent cabalFile
          return $! createVersionInfo Hackage cabalFile $! packageDescription
      return . Right $ VersionSpec (S.fromList versionInfos) pref
  where tmpDir = srcCacheDir sourceConfig </> "tmp"
