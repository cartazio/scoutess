-- | This module provides various utility functions for dealing with
--   directory manipulations

module Scoutess.Utils.Directory (copyDir) where

import System.Directory
import System.FilePath

import qualified Codec.Archive.Tar    as Tar
import qualified Data.ByteString.Lazy as L

-- | Copies the content of a directory into another
copyDir :: FilePath -> FilePath -> IO ()
copyDir from to = do
  dirContent <- filter pred `fmap` getDirectoryContents from
  taredEntries <- Tar.pack from dirContent
  Tar.unpack to $ Tar.read (Tar.write taredEntries)

  where pred x = not (x `elem` [".", ".."])