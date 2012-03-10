{-# LANGUAGE DeriveDataTypeable #-}
-- | wrapper around the 'cabal' command-line application
module Scoutess.Utils.Haddock where

import Control.Pipe
import Control.Pipe.Process
import Control.Monad.Trans  (lift)
import Data.ByteString
import Data.Typeable        (Typeable)
import Distribution.Package (PackageIdentifier(pkgName))
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Text    (display)
import Distribution.Verbosity (silent)
import System.Directory     (doesDirectoryExist)
import System.Exit          (ExitCode(..))
import System.FilePath      ((</>), (<.>))

-- | run @cabal@
haddock :: FilePath -- ^ path to directory that contains the @.cabal@
      -> [String] -- ^ arguments to pass to 'haddock'
      -> Pipe () (Either ByteString ByteString) IO ExitCode
haddock workingDirectory args =
    process "haddock" args (Just workingDirectory) Nothing
