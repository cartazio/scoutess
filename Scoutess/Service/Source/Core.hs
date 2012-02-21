{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
-- | This service is used to fetch the source for a cabal package, get changelogs (when possible), and finding out if there are changes since last time
module Scoutess.Service.Source.Core where

import Control.Monad.Error             (Error(..))
import Data.Data                       (Data, Typeable)
import Data.Text                       (Text)
import qualified Data.Text             as Text
import Distribution.PackageDescription (PackageDescription)

data SourceConfig = SourceConfig
    { srcCacheDir :: FilePath -- ^ path to directory that holds retrieved sourceg
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

-- | Information about source which has been fetched an is locally available now
data SourceInfo = SourceInfo
    { srcPath               :: FilePath           -- ^ path to the directory that contains the .cabal file
    , srcPackageDescription :: PackageDescription -- ^ contents of .cabal file
    , srcVersion            :: Text               -- ^ something that uniquely identifies this version so we can tell if there are changes from a previous version
    }
    deriving (Eq, Read, Show, Typeable)

-- | type for errors this service might encounter
data SourceException 
    = SourceErrorOther Text
    | SourceErrorUnknown
    deriving (Eq, Ord, Read, Show, Data, Typeable)

instance Error SourceException where
    noMsg    = SourceErrorUnknown
    strMsg s = SourceErrorOther (Text.pack s)
    
-- | return a human readable error message
sourceErrorMsg :: SourceException  -- ^ error
               -> Text         -- ^ error message
sourceErrorMsg (SourceErrorOther txt) = txt
sourceErrorMsg (SourceErrorUnknown)   = "unknown source error"
