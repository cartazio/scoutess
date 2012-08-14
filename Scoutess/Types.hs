-----------------------------------------------------------------------------
--
-- Module      :  Scoutess.Types
-- Copyright   :  2012 Jeremy Shaw, Alp Mestanogullari
-- License     :  BSD3
--
-- Maintainer  :  alpmestan@gmail.com
-- Stability   :
-- Portability :
--
-- | Contains the base types used in the rest of the program.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Scoutess.Types where

import Control.Arrow
import Control.Category          (Category)
import Control.Exception         (Exception)
import Control.Monad.Error       (Error(..))
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.Data                 (Typeable, Typeable2, Data(..))
import Data.Function             (on)
import Data.Monoid
import Data.Set                  (Set)
import Data.Text                 (Text, pack)

import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)

--------------------
-- Scoutess arrow --
--------------------
type Scoutess' = MaybeT (WriterT [ComponentReport] IO)

-- | A Scoutess computation is an IO function make from components. Each component may fail and\/or record
--   'ComponentReport's in addition to its return value.
newtype Scoutess a b = Scoutess {unScoutess :: Kleisli Scoutess' a b}
    deriving (Category, Arrow, ArrowApply, ArrowChoice, ArrowLoop, ArrowPlus, ArrowZero)


----------------
-- Components --
----------------
-- | A component is an IO function that may fail and\/or record log messages. It also signifies
--   if it was successful with the 'Bool'.
-- TODO: make this a newtype to ease error messages.
type Component b = MaybeT (WriterT [Text] IO) (Bool, b)

---------------------------
-- Dealing with packages --
---------------------------
-- | A package that we've seen the cabal file of but not neccesarily fetched yet
data VersionInfo = VersionInfo
    { viGPD            :: GenericPackageDescription -- ^ The parsed .cabal file
    , viVersionTag     :: Text                      -- ^ If two packages have the same name and version,
                                                    --   this is the tiebreaker
    , viSourceLocation :: SourceLocation            -- ^ Where this package came from
    } deriving Show

-- | Information about a package which has been fetched and is locally available now
data SourceInfo = SourceInfo
    { siPath         :: FilePath    -- ^ path to its location
    , siVersionInfo  :: VersionInfo -- ^ information about the package
    }
    deriving (Show, Typeable, Eq, Ord)

-- | 'Eq' and 'Ord' will ignore the 'GenericPackageDescription'. 'Ord' because it isn't an instance
--   naturally and 'Eq' in order to make 'Ord' consistent.
instance Eq VersionInfo where
    VersionInfo _ t s ==        VersionInfo _ t' s' = (t,s) ==        (t',s')
instance Ord VersionInfo where
    VersionInfo _ t s `compare` VersionInfo _ t' s' = (t,s) `compare` (t',s')

-- | Places where sources can be found.
--   TODO: Include support for torrents and Hackage-style RemoteDBs (for example, Hackage mirrors)
data SourceLocation
    = Bzr Text                            -- ^ get source from a bzr repo
    | Cd FilePath SourceLocation          -- ^ source is in the sub-directory of another 'SourceLocation'
    | Darcs Text (Maybe Text)             -- ^ get source from a darcs repo (optional tag)
    | Dir FilePath                        -- ^ get source from local directory
    | Hackage                             -- ^ get source from hackage
    | Hg Text                             -- ^ get source from mercurial
    | Patch SourceLocation Text           -- ^ Apply the patch given in the 'Text' to the target
    | Quilt SourceLocation SourceLocation -- ^ get source and apply a quilt patch
    | Svn Text                            -- ^ get source from subversion
    | Tla Text                            -- ^ get source from tla
    | Uri Text (Maybe Text)               -- ^ get source as @.tar.gz@ from uri (optional md5sum checksum)
    deriving (Read, Show, Eq, Ord, Data, Typeable)

---------------------
-- Container types --
---------------------
-- | A set of locations
data SourceSpec
    = SourceSpec { ssLocations :: Set SourceLocation }
      deriving Show

-- | Corresponds to a virtual repository
data VersionSpec = VersionSpec
    { vsVersions    :: Set VersionInfo -- ^ The packages contained in the repository
    , vsPreferences :: Maybe Text      -- ^ Repositories can also contain a file specifying preferred versions.
    }
    deriving (Show, Eq, Ord)

instance Monoid VersionSpec where
    mappend vs1 vs2 = VersionSpec ((mappend `on` vsVersions) vs1 vs2) ((mappend `on` vsPreferences) vs1 vs2)
    mempty = VersionSpec mempty mempty

-- | The finalised sources of the build
data BuildSources = BuildSources
    { targetSource :: SourceInfo   -- ^ The source for the target
    , depSources   :: [SourceInfo] -- ^ The sources for the dependencies (in reverse topological order)
    }
    deriving Show

-----------
-- Other --
-----------
-- | Configuration for fetching sources
data SourceConfig = SourceConfig
    { srcCacheDir :: FilePath -- ^ path to directory that holds retrieved source
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

-- | The data type representing a local package repository
data LocalHackage = LocalHackage
    { hackageDir    :: FilePath
    , hackageTmpDir :: FilePath
    } deriving Show

-- | type for errors this service might encounter
--
-- should this be an 'Exception' or just an 'Error'?
--
-- Do we need to include the 'SourceLocation' in the error?
data SourceException
    = SourceErrorOther Text
    | SourceErrorUnknown
    deriving (Eq, Ord, Read, Show, Data, Typeable)

instance Error SourceException where
    noMsg    = SourceErrorUnknown
    strMsg s = SourceErrorOther (pack s)

instance Exception SourceException

-- | The information we need in order to build a package
--   TODO: just including the 'TargetSpec' is a bit lazy, we probably should
--   extract the fields we need instead.
data BuildSpec = BuildSpec
    { bsTargetSpec    :: TargetSpec     -- ^ input to scoutess
    , bsTargetInfo    :: VersionInfo    -- ^ information about the target package
    , bsDependencies  :: [VersionInfo]  -- ^ in reverse topological order
    , bsToBuild       :: Bool           -- ^ is there a build needed?
    }
    deriving Show

-- | The initial input to scoutess. A bit of a mess.
data TargetSpec = TargetSpec
    { tsName            :: Text           -- ^ the name of the target package
    , tsVersion         :: Text           -- ^ the version of the target package
    , tsLocation        :: SourceLocation -- ^ the 'SourceLocation' of the target package
    , tsTmpDir          :: FilePath       -- ^ a directory that scoutess will put temporary files in
    , tsLocalHackage    :: LocalHackage   -- ^ the local hackage repo
    , tsPackageDB       :: FilePath       -- ^ the directory to be used as a package-db
    , tsSourceConfig    :: SourceConfig   -- ^ the directory to unpack things from repos in
    , tsCustomCabalArgs :: [Text]         -- ^ any additional arguments to pass to cabal
    }
    deriving Show

-- | Each component can report success or (non-fatal) failure along with extra information
data ComponentReport = ComponentReport
    { componentName    :: Text
    , componendSuccess :: Bool
    , componentOutput  :: Text
    } deriving Show

-- TODO: should we just have a link to the LocalBuildInfo?
data BuildReport = BuildReport
    { brBuildSpec      :: BuildSpec
    , brLocalBuildInfo :: LocalBuildInfo
    }
    deriving Show

-- | A cut down 'BuildReport' containing a record of which packages were built
data PriorRun = PriorRun
    { prTarget       :: VersionInfo
    , prDependencies :: Set VersionInfo
    }
    deriving Show

-- | A link to a 00-index.tar.gz file
data LocalHackageIndex = LocalHackageIndex
    { pathToIndex :: FilePath
    }
    deriving Show
