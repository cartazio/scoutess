module Scoutess.Utils.Cabal where

import Control.Pipe
import Control.Pipe.Process
import Data.ByteString
import Data.Void
import System.Exit

cabal :: FilePath -- ^ path to directory that contains the @.cabal@
      -> [String] -- ^ arguments to pass to 'cabal'
      -> Pipe Void (Either ByteString ByteString) IO ExitCode
cabal workingDirectory args =
    process "cabal" args (Just workingDirectory) Nothing

configure :: FilePath -- ^ path to directory that contains the @.cabal@
          -> [String] -- ^ flags to pass to @cabal configure@
          -> Pipe Void (Either ByteString ByteString) IO ExitCode
configure workingDirectory args =
    cabal workingDirectory  ("configure":args)

haddock :: FilePath -- ^ path to directory that contains the @.cabal@
        -> [String] -- ^ flags to pass to @cabal haddock@
        -> Pipe Void (Either ByteString ByteString) IO ExitCode
haddock workingDirectory args =
    cabal workingDirectory  ("haddock":args)

