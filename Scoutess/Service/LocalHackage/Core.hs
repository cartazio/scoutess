-- | This service is used to have a sort of local hackage-like package index, which basically means maintaining a package index containing the dependencies

module Scoutess.Service.LocalHackage.Core (generateIndex, addPackage, LocalHackage(..)) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZ
import qualified Data.ByteString.Lazy as L

import Control.Monad                   (liftM, when)
import Data.Text                       (Text, unpack)
import Distribution.Package
import Distribution.PackageDescription
import System.Directory
import System.FilePath                 ((</>), (<.>))
import System.FilePath.Find            (always, find, fileName, extension, (==?))

import Scoutess.Service.Source.Core
import Scoutess.Utils.Archives
import Scoutess.Utils.Directory

-- | The data type representing a local package repository
data LocalHackage = LocalHackage { hackageDir    :: FilePath
                                 , hackageTmpDir :: FilePath
                                 }

-- | generates a package index from a list of package archives
generateIndex :: LocalHackage -- ^ directory that contains the packages
              -> FilePath     -- ^ filepath for the output package index
              -> IO ()
generateIndex hackage pkgIndex = do
  let pkgsDir = hackageDir hackage
  cabals <- findCabalFiles pkgsDir
  tarFiles (map (drop $ prefix pkgsDir) cabals) pkgsDir pkgIndex
  tarGzipFiles (map (drop $ prefix pkgsDir) cabals) pkgsDir (pkgIndex ++ ".gz")

  where prefix d = case last d == '/' of
          True  -> length d
          False -> length d + 1

-- | extracts a list of package (.tar.gz) archives to a given directory
extractArchives :: [FilePath] -- ^ list of package archives (.tar.gz)
                -> FilePath   -- ^ output directory
                -> IO ()
extractArchives archives dir = mapM_ (flip extractArchive dir) archives

-- | looks for all .cabal files in the provided directory and its subdirectories
findCabalFiles :: FilePath      -- ^ where to start looking (recursively)
               -> IO [FilePath] -- ^ the file paths to the .cabal files
findCabalFiles = find recPred (extension ==? ".cabal")
  where recPred = (`notElem` ["_darcs", ".git", "src", "tests", "test", "examples", "Data", "Control", "data"]) `liftM` fileName

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
    createDirectory packageVersionDir
    copyFile packageCabalFilePath $ packageVersionDir </> pkgN <.> ".cabal"
    createDirectory tmpPackageDir
    copyDir src tmpPackageDir
    tarGzipFiles [pkgIdent] (hackageTmpDir hackage) (packageVersionDir </> pkgIdent <.> ".tar.gz")
    removeDirectoryRecursive tmpPackageDir
    generateIndex hackage (hackageDir hackage </> "00-index.tar")

  where (PackageName pkgN)    = pkgName . package . srcPackageDescription $ srcInfo
        pkgVersion               = srcVersion srcInfo
        tmpPackageDir            = hackageTmpDir hackage </> pkgIdent
        packageDir               = hackageDir hackage </> pkgN
        packageVersionDir        = packageDir </> unpack pkgVersion
        packageCabalFilePath     = src </> pkgN <.> ".cabal"
        packageArchivePath       = packageVersionDir </> pkgIdent <.> ".tar.gz"
        src                      = srcPath srcInfo
        pkgIdent                 = pkgN ++ "-" ++ unpack pkgVersion