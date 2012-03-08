{-# LANGUAGE DeriveDataTypeable #-}
-- | wrapper around the 'cabal' command-line application
module Scoutess.Utils.Cabal where

import Control.Pipe
import Control.Pipe.Process
import Control.Monad.Trans  (lift)
import Data.ByteString
import Data.Typeable        (Typeable)
import Distribution.Package (PackageIdentifier)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Text    (display)
import Distribution.Verbosity (silent)
import System.Exit          (ExitCode(..))
import System.FilePath      ((</>), (<.>))

-- | run @cabal@
cabal :: FilePath -- ^ path to directory that contains the @.cabal@
      -> [String] -- ^ arguments to pass to 'cabal'
      -> Pipe () (Either ByteString ByteString) IO ExitCode
cabal workingDirectory args =
    process "cabal" args (Just workingDirectory) Nothing

-- | run @cabal configure@
configure :: FilePath -- ^ path to directory that contains the @.cabal@
          -> [String] -- ^ flags to pass to @cabal configure@
          -> Pipe () (Either ByteString ByteString) IO ExitCode
configure workingDirectory args =
    cabal workingDirectory  ("configure":args)

-- | run @cabal haddock@
haddock :: FilePath -- ^ path to directory that contains the @.cabal@
        -> [String] -- ^ flags to pass to @cabal haddock@
        -> Pipe () (Either ByteString ByteString) IO ExitCode
haddock workingDirectory args =
    cabal workingDirectory  ("haddock":args)

-- | run @cabal update@
update :: Pipe () (Either ByteString ByteString) IO ExitCode
update =
    cabal "." ["update"]

-- | run @cabal unpack@
unpack :: PackageIdentifier  -- ^ package to fetch and unpack
       -> String             -- ^ directory to run @cabal unpack@ in
       -> Pipe () (Either ByteString ByteString) IO (Either ExitCode UnpackInfo)
unpack packageIdentifier workingDirectory =
    do exitCode <- cabal workingDirectory ["unpack", display packageIdentifier]
       case exitCode of
         (ExitFailure _) ->
                return (Left exitCode)
         (ExitSuccess)   ->
             do let unpackDir = workingDirectory </> display packageIdentifier
                    dotCabal  = unpackDir </> display packageIdentifier <.> "cabal"
                pkgDesc <- lift $ readPackageDescription silent dotCabal
                let unpackInfo =
                        UnpackInfo { unpackPath = unpackDir
                                   , unpackPackageDescription = pkgDesc
                                   }
                return (Right unpackInfo)


-- | Information about source which has been unpacked via 'cabal unpack'
data UnpackInfo = UnpackInfo
    { unpackPath               :: FilePath                  -- ^ path to the directory that contains the .cabal file
    , unpackPackageDescription :: GenericPackageDescription -- ^ contents of .cabal file
    }
    deriving (Eq, Show, Typeable)
