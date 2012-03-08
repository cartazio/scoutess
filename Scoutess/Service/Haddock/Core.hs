{-# LANGUAGE RecordWildCards #-}
module Scoutess.Service.Haddock.Core where

import Control.Pipe
import Control.Pipe.Binary
import Control.Pipe.Process
import Control.Monad.Trans
import Data.ByteString.Char8 (pack)
import Distribution.Package (PackageIdentifier, PackageName)
import Distribution.PackageDescription (GenericPackageDescription(..), PackageDescription(..))
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import Distribution.Text    (display)
import Distribution.Version (Version)
import Scoutess.Utils.Cabal (UnpackInfo(..))
import qualified Scoutess.Utils.Cabal as Cabal
import System.Directory     (createDirectoryIfMissing)
import System.FilePath      ((</>))
import System.IO

haddock :: FilePath           -- ^ directory for unpacked source (from hackage)
        -> FilePath           -- ^ directory for generated haddock docs
        -> PackageIdentifier  -- ^ package to generate docs for
        -> IO ()
haddock unpackDir docDir packageIdentifier =
    runPipe $
      (do lift $ createDirectoryIfMissing True unpackDir
          -- ec <- Cabal.update -- check exit code
          -- TODO: check if unpacked source already exists
          r <- Cabal.unpack unpackDir packageIdentifier
          case r of
            (Left ec) ->
                   yield (Left $ pack $ show ec)
            (Right unpackInfo) ->
                do ec  <- Cabal.configure (unpackPath unpackInfo) ["--user"]
                   lbi <- lift $ getPersistBuildConfig (unpackPath unpackInfo </> "dist") -- I don't feel good about hardcoding "dist"
                   case libraryConfig lbi of
                     Nothing -> return () -- nothing to do if there is no library?
                     (Just clbi) ->
                         do let packageNames = map snd (componentPackageDeps clbi)
                            lift $ print packageNames
                            let docDir' = docDir </> display packageIdentifier
                            lift $ createDirectoryIfMissing True docDir'
                            ec <- Cabal.haddock (unpackPath unpackInfo)
                                       [ "--haddock-option", "-o"              , "--haddock-option", docDir'
                                       , "--haddock-option", "--read-interface", "--haddock-option", "../foo-0.1,/tmp/doc-dir/foo-0.1/foo.haddock"
                                       , "--html"
                                       , "--hoogle"
                                       ]
                            return ()
          return ()
      ) >+> joinP >+> handleWriter stdout
