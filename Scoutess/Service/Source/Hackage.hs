{-# LANGUAGE OverloadedStrings #-}
-- | fetch packages from hackage using @Network.HTTP@

module Scoutess.Service.Source.Hackage where

import Control.Monad.Trans                           (MonadIO(..), liftIO)
import Data.List                                     (sort)
import Data.Maybe                                    (maybe)
import Data.Text                                     (Text)
import qualified Data.Text                           as Text
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription.Parse         (readPackageDescription)
import Distribution.Verbosity                        (silent)
import System.FilePath.Posix                         ((</>))
import System.Directory                              (createDirectoryIfMissing, renameDirectory, doesDirectoryExist, getDirectoryContents, removeDirectoryRecursive)

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
      mVer <- getLatestVersionOf sourceConfig pkgName
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
  dledPath <- liftIO $ downloadFile pkgUrl $ (srcCacheDir sourceConfig) ++ Text.unpack pkgName ++ "-" ++ Text.unpack pkgVersion ++ ".tar.gz"
  case dledPath of 
    Just _ -> do 
      let destDir = srcCacheDir sourceConfig </> Text.unpack pkgName
      liftIO $ createDirectoryIfMissing True destDir
      liftIO $ extractArchive localPath destDir
      liftIO $ renameDirectory (destDir </> Text.unpack pkgName ++ "-" ++ Text.unpack pkgVersion) (destDir </> Text.unpack pkgVersion)
      genericPkgDesc <- liftIO $ readPackageDescription silent (destDir </> Text.unpack pkgVersion </> (Text.unpack pkgName ++ ".cabal")) 
      let pkgDescr = flattenPackageDescription genericPkgDesc
      let srcVer = pkgName `Text.append` "-" `Text.append` pkgVersion
      return . Right $ SourceInfo { srcPath               = destDir </> Text.unpack pkgVersion
                                  , srcPackageDescription = pkgDescr
                                  , srcVersion            = srcVer }
    Nothing -> return . Left $ SourceErrorOther "Couldn't download the package archive. Please check that your connection and the hackage.haskell.org server are up."
    
-- | Gets the latest version (on hackage.haskell.org) of a given package.
--   Returns 'Nothing' if it can't retrieve it.
-- /!\ THIS FUNCTIONS FETCHES THE PKGINDEX, INSPECTS IT AND THEN DOESNT REMOVES IT (it's removed by fetchHackage)
--     ^ EVERY TIME IT'S CALLED!        
getLatestVersionOf :: (MonadIO m) =>
                      SourceConfig -- ^ 'SourceConfig'
                   -> Text -- ^ Package we want the latest version of
                   -> m (Maybe Text)
getLatestVersionOf sourceConfig pkgName = do
  liftIO $ createDirectoryIfMissing True tmpDir
  pkgIndex' <- liftIO $ downloadFile "http://hackage.haskell.org/packages/archive/00-index.tar.gz" $ tmpDir </> "00-index.tar.gz"
  case pkgIndex' of 
    Nothing -> return Nothing
    Just pkgIndex -> do
      liftIO $ extractArchive pkgIndex tmpDir
      pkgExists <- liftIO $ doesDirectoryExist tmpPkgDir
      case pkgExists of
        True -> do
          vers <- liftIO $ getDirectoryContents tmpPkgDir 
          return $ if null vers then Nothing else Just . Text.pack . last $ sort vers
        False -> return Nothing
    
  where tmpDir = srcCacheDir sourceConfig </> "tmp"
        tmpPkgDir = tmpDir </> Text.unpack pkgName