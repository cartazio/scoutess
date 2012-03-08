{-# LANGUAGE RecordWildCards #-}
module Scoutess.Service.Haddock.Core where

import Control.Pipe
import Control.Pipe.Binary
import Control.Pipe.Process
import Control.Monad.Trans
import Data.ByteString.Char8 (pack)
import Distribution.Package (PackageIdentifier, PackageName)
import Distribution.Version (Version)
import Scoutess.Utils.Cabal (UnpackInfo(..))
import qualified Scoutess.Utils.Cabal as Cabal
import System.Directory     (createDirectoryIfMissing)
import System.IO

haddock :: FilePath           -- ^ directory for unpacked source (from hackage)
        -> FilePath           -- ^ directory for generated haddock docs
        -> PackageIdentifier  -- ^ package to generate docs for
        -> IO ()
haddock unpackDir docDir packageIdentifier =
    runPipe $
      (do lift $ createDirectoryIfMissing True unpackDir
          lift $ createDirectoryIfMissing True docDir
          -- ec <- Cabal.update -- check exit code
          -- TODO: check if unpacked source already exists
          r <- Cabal.unpack unpackDir packageIdentifier
          case r of
            (Left ec) ->
                   yield (Left $ pack $ show ec)
            (Right unpackInfo ) ->
                do ec <- Cabal.configure (unpackPath unpackInfo) ["--builddir", docDir, "--user"]
                   ec <- Cabal.haddock (unpackPath unpackInfo) ["--builddir", docDir, "--html", "--hoogle"]
                   return ()
          return ()
      ) >+> joinP >+> handleWriter stdout





