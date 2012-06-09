{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, TemplateHaskell #-}
-- | This service is used to fetch the source for a cabal package, get changelogs (when possible), and finding out if there are changes since last time.
module Scoutess.Service.Source.Core where

import Control.Monad.Error             (Error(..))
import Control.Exception               (Exception)
import Data.Data                       (Data, Typeable)
import Data.Function                   (on)
import Data.SafeCopy                   (SafeCopy(..), base, deriveSafeCopy)
import Data.Text                       (Text)
import qualified Data.Text             as Text
import Distribution.PackageDescription (PackageDescription)

-- | Configuration for fetching sources
data SourceConfig = SourceConfig
    { srcCacheDir :: FilePath -- ^ path to directory that holds retrieved source
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

-- | where to find the remote source for a cabal package
data SourceLocation
    = Bzr Text                            -- ^ get source from a bzr repo
    | Cd FilePath SourceLocation          -- ^ source is in the sub-directory of another 'SourceLocation'
    | Darcs Text (Maybe Text)             -- ^ get source from darcs (optional tag)
    | Dir FilePath                        -- ^ get source from local directory
    | Hackage Text (Maybe Text)           -- ^ get source from hackage (optional version)
    | Hg Text                             -- ^ get source from mercurial
    | Quilt SourceLocation SourceLocation -- ^ get source and apply a quilt patch
    | Svn Text                            -- ^ get source from subversion
    | Tla Text                            -- ^ get source from tla
    | Uri Text (Maybe Text)               -- ^ get source as @.tar.gz@ from uri (optional md5sum checksum)
    deriving (Read, Show, Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''SourceLocation)

-- | Information about source which has been fetched an is locally available now
data SourceInfo = SourceInfo
    { srcPath               :: FilePath           -- ^ path to the directory that contains the .cabal file
    , srcPackageDescription :: PackageDescription -- ^ contents of .cabal file
    , srcVersion            :: Text               -- ^ something that uniquely identifies this version so we can tell if there are changes from a previous version
    }
    deriving (Read, Show, Typeable)

instance Eq SourceInfo where
   (==) = (==) `on` srcVersion

instance SafeCopy PackageDescription where
    getCopy = undefined
    putCopy = undefined

$(deriveSafeCopy 0 'base ''SourceInfo)

-- | type for errors this service might encounter
--
-- should this be an 'Exception' or just an 'Error'?
--
-- Do we need to include the 'SourceLocation' in the error?
data SourceException
    = SourceErrorOther Text
    | SourceErrorUnknown
    deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''SourceException)

instance Error SourceException where
    noMsg    = SourceErrorUnknown
    strMsg s = SourceErrorOther (Text.pack s)

instance Exception SourceException

-- | return a human readable error message
sourceErrorMsg :: SourceException  -- ^ error
               -> Text         -- ^ error message
sourceErrorMsg (SourceErrorOther txt) = txt
sourceErrorMsg (SourceErrorUnknown)   = "unknown source error"
