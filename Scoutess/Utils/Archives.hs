module Scoutess.Utils.Archives where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZ
import qualified Data.ByteString.Lazy as L

extractArchive :: FilePath -- ^ input .tar.gz archive
               -> FilePath -- ^ output directory
               -> IO ()
extractArchive ar dir = L.readFile ar >>= Tar.unpack dir . Tar.read . GZ.decompress

tarFiles :: [FilePath] -- ^ list of files to put in the archive
         -> FilePath -- ^ base directory
         -> FilePath -- ^ file path for the output tar file
         -> IO ()
tarFiles files baseDir output = Tar.create output baseDir $ map (drop n) files 
  where n = let l = length baseDir in
            if last baseDir == '/' then l else l+1
                                               
gzipFile :: FilePath -- ^ input file to be gzip'ed
         -> FilePath -- ^ output file
         -> IO ()
gzipFile input output = L.readFile input >>= L.writeFile output . GZ.compress