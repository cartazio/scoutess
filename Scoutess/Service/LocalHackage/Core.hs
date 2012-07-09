-- | This service is used to have a sort of local hackage-like package index, which basically means maintaining a package index containing the dependencies

module Scoutess.Service.LocalHackage.Core (generateIndex, generateIndexSelectively, addPackage, LocalHackage(..)) where

import Control.Monad                   (when)
import Data.List                       (isSuffixOf)
import Data.Text                       (unpack, pack)
import Data.Version
import System.Directory
import System.FilePath                 ((</>), (<.>))

import Scoutess.Core
import Scoutess.Utils.Archives
import Scoutess.Utils.Directory

-- | generates a package index from a list of package archives
generateIndex :: LocalHackage -- ^ directory that contains the packages
              -> FilePath     -- ^ filepath for the output package index
              -> IO ()
generateIndex = generateIndexSelectively Nothing

-- | generates a package index from a (filtered) list of package archives
generateIndexSelectively :: Maybe [VersionInfo] -- ^ if included, only put these packages in the index
                         -> LocalHackage        -- ^ directory that contains the packages
                         -> FilePath            -- ^ filepath for the output package index
                         -> IO ()
generateIndexSelectively mVersionInfos hackage pkgIndex = do
  let pkgsDir = hackageDir hackage
  cabals <- findCabalFiles pkgsDir
  -- assume the cabal files have the filepath of .../pkgVersion/pkgName.cabal
  let cabals' = maybe cabals (flip filter cabals . validCabal) mVersionInfos
  tarFiles (map (drop $ prefix pkgsDir) cabals') pkgsDir pkgIndex
  tarGzipFiles (map (drop $ prefix pkgsDir) cabals') pkgsDir (pkgIndex ++ ".gz")
  where prefix d = case last d == '/' of
          True  -> length d
          False -> length d + 1
        toCabalDir :: VersionInfo -> FilePath
        toCabalDir vi = showVersion (viVersion vi) </> viName vi <.> ".cabal"
        validCabal :: [VersionInfo] -> FilePath -> Bool
        validCabal versionInfos cabal = any (flip isSuffixOf cabal . toCabalDir) versionInfos

-- | extracts a list of package (.tar.gz) archives to a given directory
extractArchives :: [FilePath] -- ^ list of package archives (.tar.gz)
                -> FilePath   -- ^ output directory
                -> IO ()
extractArchives archives dir = mapM_ (flip extractArchive dir) archives

-- | Adds the package described by the given @SourceInfo@ to the hackage db
--   If the same package and version already are in the db, does nothing
--   (it assumes there's a .cabal file in the @SourceInfo@'s srcPath)
addPackage :: SourceInfo   -- ^ source info we get after fetching a package
           -> LocalHackage -- ^ hackage repository
           -> IO ()
addPackage srcInfo hackage = do
  createDirectoryIfMissing True packageDir
  pkgVersionExists <- doesDirectoryExist packageVersionDir
  when (not pkgVersionExists) $ do
    createDirectoryIfMissing True packageVersionDir
    copyFile packageCabalFilePath $ packageVersionDir </> pkgN <.> ".cabal"
    createDirectoryIfMissing True tmpPackageDir
    copyDir src tmpPackageDir
    tarGzipFiles [pkgIdent] (hackageTmpDir hackage) (packageVersionDir </> pkgIdent <.> ".tar.gz")
    removeDirectoryRecursive tmpPackageDir
    generateIndex hackage (hackageDir hackage </> "00-index.tar")
  where pkgN                 = srcName srcInfo
        pkgV                 = pack . showVersion $ srcVersion srcInfo
        tmpPackageDir        = hackageTmpDir hackage </> pkgIdent
        packageDir           = hackageDir hackage </> pkgN
        packageVersionDir    = packageDir </> unpack pkgV
        packageCabalFilePath = src </> pkgN <.> ".cabal"
        packageArchivePath   = packageVersionDir </> pkgIdent <.> ".tar.gz"
        src                  = srcPath srcInfo
        pkgIdent             = pkgN ++ "-" ++ unpack pkgV
