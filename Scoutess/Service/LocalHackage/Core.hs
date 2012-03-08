-- | This service is used to have a sort of local hackage-like package index, which basically means maintaining a package index containing the dependencies
module Scoutess.Service.LocalHackage.Core where

import Control.Monad (liftM)
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZ
import qualified Data.ByteString.Lazy as L
import System.FilePath.Find (always, find, fileName, extension, (==?))

import Scoutess.Utils.Archives

-- | generates a package index from a list of package archives
generateIndex :: [FilePath] -- ^ list of .tar.gz files for packages
              -> FilePath   -- ^ directory in which packages should be extracted
              -> FilePath   -- ^ file path for the output package index file
              -> IO ()
generateIndex archives pkgsDir pkgFile = do
  extractArchives archives pkgsDir
  cabals <- findCabalFiles pkgsDir
  tarCabalFiles cabals pkgsDir pkgFile

-- | extracts a list of package (.tar.gz) archives to a given directory
extractArchives :: [FilePath] -- ^ list of package archives (.tar.gz)
                -> FilePath   -- ^ output directory
                -> IO ()
extractArchives archives dir = mapM_ (flip extractArchive dir) archives

-- | looks for all .cabal files in the provided directory and its subdirectories
findCabalFiles :: FilePath -- ^ where to start looking (recursively)
               -> IO [FilePath] -- ^ the file paths to the .cabal files
findCabalFiles = find recPred (extension ==? ".cabal")
  where recPred = (`notElem` ["_darcs", ".git", "src", "tests", "test", "examples", "Data", "Control", "data"]) `liftM` fileName

-- | compresses all the cabal files in a tar archive, keeping the directory tree structure
tarCabalFiles :: [FilePath] -- ^ list of cabal files
              -> FilePath   -- ^ base directory
              -> FilePath   -- ^ file path for the output tar file
              -> IO ()
tarCabalFiles = tarFiles
