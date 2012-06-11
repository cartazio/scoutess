{-# LANGUAGE OverloadedStrings #-}
-- | fetch packages from hackage using @Network.HTTP@

module Scoutess.Service.Source.Hackage (fetchHackage, fetchAllVersions) where

import Control.Monad                                 (forM, liftM)
import Control.Monad.Trans                           (MonadIO(..), liftIO)
import Data.List                                     (sort)
import Data.Maybe                                    (catMaybes)
import Data.Text                                     (Text)
import qualified Data.Text                           as Text
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription.Parse         (readPackageDescription)
import qualified Data.Set                            as S
import Distribution.Verbosity                        (silent)
import System.FilePath.Posix                         ((</>))
import System.Directory                              (createDirectoryIfMissing, renameDirectory, doesDirectoryExist, getDirectoryContents, removeDirectoryRecursive, removeFile)

import Scoutess.Service.Source.Core (SourceConfig(..), SourceException(..), SourceInfo(..))
import Scoutess.Utils.Archives
import Scoutess.Utils.HTTP

baseUrl :: String
baseUrl = "http://hackage.haskell.org/packages/archive/"

packageUrl :: Text -> Text -> String
packageUrl pkgName pkgVersion = baseUrl ++ Text.unpack pkgName ++ "/" ++ Text.unpack pkgVersion ++ "/" ++ Text.unpack pkgName ++ "-" ++ Text.unpack pkgVersion ++ ".tar.gz"

-- | A wrapper that handles unspecified version, etc
fetchHackage :: (MonadIO m) =>
                SourceConfig -- ^ 'SourceConfig'
             -> Text         -- ^ package name
             -> Maybe Text   -- ^ package version
             -> m (Either SourceException SourceInfo)
fetchHackage sourceConfig pkgName pkgVersion' =
  case pkgVersion' of
    Nothing -> do
      mVer <- fetchLatestVersionOf sourceConfig pkgName
      liftIO $ removeDirectoryRecursive (srcCacheDir sourceConfig </> "tmp")
      maybe (return . Left $ SourceErrorOther "Couldn't find package or latest package version") (fetchHackage' sourceConfig pkgName) mVer
    Just ver -> fetchHackage' sourceConfig pkgName ver

-- | The function that actually fetches the package
fetchHackage' :: (MonadIO m) =>
                 SourceConfig
              -> Text
              -> Text
              -> m (Either SourceException SourceInfo)
fetchHackage' sourceConfig pkgName pkgVersion = do
  let pkgUrl = packageUrl pkgName pkgVersion
  let localPath = srcCacheDir sourceConfig ++ Text.unpack pkgName ++ "-" ++ Text.unpack pkgVersion ++ ".tar.gz"
  dledPath <- liftIO $ downloadFile pkgUrl localPath
  case dledPath of
    Just _ -> do
      let destDir = srcCacheDir sourceConfig </> Text.unpack pkgName
      liftIO $ createDirectoryIfMissing True destDir
      liftIO $ extractArchive localPath destDir
      liftIO $ renameDirectory (destDir </> Text.unpack pkgName ++ "-" ++ Text.unpack pkgVersion) (destDir </> Text.unpack pkgVersion)
      liftIO $ removeFile localPath
      genericPkgDesc <- liftIO $ readPackageDescription silent (destDir </> Text.unpack pkgVersion </> (Text.unpack pkgName ++ ".cabal"))
      let pkgDescr = flattenPackageDescription genericPkgDesc
      let srcVer = pkgVersion
      return . Right $ SourceInfo { srcPath               = destDir </> Text.unpack pkgVersion
                                  , srcPackageDescription = pkgDescr
                                  , srcVersion            = srcVer }
    Nothing -> return . Left $ SourceErrorOther "Couldn't download the package archive. Please check that your connection and the hackage.haskell.org server are up."

-- | returns a list of (name,versions)
-- maybe it should return a list of VersionInfos instead?
-- XXX: /!\ THIS FUNCTIONS FETCHES THE PKGINDEX, INSPECTS IT AND THEN DOESN'T REMOVE IT
--     ^ EVERY TIME IT'S CALLED!
fetchAllVersions :: (MonadIO m) =>
                  SourceConfig -- ^ 'SourceConfig' defining where the 00-index is stored
               -> m (S.Set (Text,Text))
fetchAllVersions sourceConfig = do
  liftIO $ createDirectoryIfMissing True tmpDir
  pkgIndex' <- liftIO $ downloadFile "http://hackage.haskell.org/packages/archive/00-index.tar.gz" $ tmpDir </> "00-index.tar.gz"
  pkgPairs  <- case pkgIndex' of
    Nothing       -> return []
    Just pkgIndex -> do
      liftIO $ extractArchive pkgIndex tmpDir
      pkgs       <- liftIO $ getDirectoryContents tmpDir
      maybePairs <- liftIO $ forM pkgs (\pkgName -> do
        let pkgDir = tmpDir </> pkgName
        pkgExists <- liftIO $ doesDirectoryExist pkgDir
        case pkgExists of
          True -> do
            vers <- liftIO $ getDirectoryContents pkgDir
            return $ Just (Text.pack pkgName, map Text.pack vers)
          False -> return Nothing)
      return (catMaybes maybePairs)
  return . S.fromList . explode $ pkgPairs
  where tmpDir = srcCacheDir sourceConfig </> "tmp"
        explode :: [(a,[b])] -> [(a,b)]
        explode = concatMap (\(a,bs) -> map (\b -> (a,b)) bs)

-- | Gets the latest version (on hackage.haskell.org) of a given package.
--   Returns 'Nothing' if it can't retrieve it.
fetchLatestVersionOf :: (MonadIO m) =>
                      SourceConfig -- ^ 'SourceConfig'
                   -> Text -- ^ Package we want the latest version of
                   -> m (Maybe Text)
fetchLatestVersionOf sourceConfig pkgName = do
  nameVers <- fetchAllVersions sourceConfig
  let vers = S.filter ((==) pkgName . fst) $ nameVers
  return $ liftM (snd . fst) . S.maxView $ vers
