-- |This module provides some utility functions for working with @tar@ and @gzip@.
module Scoutess.Utils.Archives where

import qualified Codec.Archive.Tar      as Tar
import qualified Codec.Compression.GZip as GZ
import qualified Data.ByteString.Lazy   as L

-- | extract a @.tar.gz@ archive
extractArchive :: FilePath -- ^ input .tar.gz archive
               -> FilePath -- ^ output directory
               -> IO ()
extractArchive ar dir = do
    archive <- L.readFile ar
    -- the length is forced to close the handle to ar
    L.length archive `seq` Tar.unpack dir . Tar.read $ GZ.decompress archive

-- | create a @.tar@ file from /relative/ file paths
tarFiles :: [FilePath] -- ^ list of files to put in the archive
         -> FilePath -- ^ base directory
         -> FilePath -- ^ file path for the output tar file
         -> IO ()
tarFiles files baseDir output = Tar.create output baseDir files

-- | unpacks a tar archive
extractTar :: FilePath -- ^ directory to extract files in
           -> FilePath -- ^ path to the tar archive
           -> IO ()
extractTar = Tar.extract

-- | compress a file using @gzip@
gzipFile :: FilePath -- ^ input file to be gzip'ed
         -> FilePath -- ^ output file
         -> IO ()
gzipFile input output =
    L.readFile input >>= L.writeFile output . GZ.compress

-- | creates a @.tar.gz@ archive
tarGzipFiles :: [FilePath] -- ^ list of files or subdirs to put in the archive
             -> FilePath -- ^ base directory
             -> FilePath -- ^ file path for the output .tar.gz file
             -> IO ()
tarGzipFiles files baseDir output = Tar.pack baseDir files >>= (L.writeFile output . GZ.compress . Tar.write)