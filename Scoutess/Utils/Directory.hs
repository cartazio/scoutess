-- | This module provides various utility functions for dealing with
--   directory manipulations

module Scoutess.Utils.Directory (copyDir) where

import System.Directory
import System.FilePath

import Scoutess.Utils.Archives (tarFiles, extractTar)

-- | Copies the content of a directory into another
copyDir :: FilePath -> FilePath -> IO ()
copyDir from to = do
  dirContent <- filter pred `fmap` getDirectoryContents from
  tarFiles dirContent from (from </> "tmp.tar")
  extractTar to (from </> "tmp.tar")
  removeFile (from </> "tmp.tar")

  where pred x = not (x `elem` [".", ".."])