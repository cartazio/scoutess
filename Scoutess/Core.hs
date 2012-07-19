{-# LANGUAGE GeneralizedNewtypeDeriving
           , DeriveDataTypeable
           , OverloadedStrings
           , StandaloneDeriving #-}

module Scoutess.Core where

import Control.Arrow
import Control.Applicative  ((<$>))
import Control.Category     (Category)
import Control.Exception    (Exception)
import Control.Monad.Error  (Error(..))
import Data.Bimap           (Bimap)
import qualified Data.Bimap as B
import Data.Data            (Typeable, Typeable2, Data(..), mkNoRepType, gcast2)
import Data.Function        (on)
import Data.Graph           (Graph, Vertex)
import Data.Set             (Set)
import qualified Data.Set as S
import Data.Text            (Text)
import qualified Data.Text as T
import Data.Version         (showVersion)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.Version (Version)
import System.FilePath.Find (find, fileName, extension, (==?), depth)

--------------------
-- Scoutess arrow --
--------------------

newtype Scoutess a b = Scoutess {unwrapScoutess :: (Kleisli IO a b)}
    deriving (Category, Arrow, ArrowApply, ArrowChoice, ArrowLoop, ArrowPlus, ArrowZero)

runScoutess :: Scoutess a b -> a -> IO b
runScoutess = runKleisli . unwrapScoutess

liftScoutess :: (a -> IO b) -> Scoutess a b
liftScoutess = Scoutess . Kleisli

---------------------------
-- Dealing with packages --
---------------------------

-- | Information about a package that we've seen the cabal file of but not neccesarily fetched yet
data VersionInfo = VersionInfo
    { viPackageDescription :: PackageDescription
    , viVersionTag         :: Text
      -- ^ a unique version identifier.
      --   For packages from Hackage, this is equal to "<name>-<version>"
    , viSourceLocation     :: SourceLocation
    } deriving (Show, Read)

-- | Information about a package which has been fetched and is locally available now
data SourceInfo = SourceInfo
    { srcPath         :: FilePath           -- ^ path to the directory that contains the .cabal file
    , srcVersionInfo  :: VersionInfo        -- ^ information about the source
    }
    deriving (Read, Show, Typeable)

-- | 'Eq' and 'Ord' will ignore the 'PackageDescription'. 'Ord' because it isn't an instance
--   naturally and 'Eq' in order to make 'Ord' consistent.
instance Eq VersionInfo where
    VersionInfo _ t s ==        VersionInfo _ t' s' = (t,s) ==        (t',s')
instance Ord VersionInfo where
    VersionInfo _ t s `compare` VersionInfo _ t' s' = (t,s) `compare` (t',s')

-- | Places where sources can be found
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

data SourceSpec
    = SourceSpec { locations :: Set SourceLocation }
      deriving Show

data VersionSpec = VersionSpec
    { versions :: Set VersionInfo
    }
    deriving (Show, Eq, Ord)

combineVersionSpecs :: [VersionSpec] -> VersionSpec
combineVersionSpecs = VersionSpec . S.unions . map versions

-----------------
-- Other types --
-----------------

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
    strMsg s = SourceErrorOther (T.pack s)

instance Exception SourceException

-- | return a human readable error message
sourceErrorMsg :: SourceException -- ^ error
               -> Text            -- ^ error message
sourceErrorMsg (SourceErrorOther txt) = txt
sourceErrorMsg (SourceErrorUnknown)   = "unknown source error"

-- TODO: just including the TargetSpec seems like a hack, which
-- parts of it are actually used?
data BuildSpec = BuildSpec
    { bsTargetSpec   :: TargetSpec
    , bsNewDeps      :: Set VersionInfo
    , bsAllDeps      :: Set VersionInfo
    }
    deriving Show

-- | The initial input to scoutess
data TargetSpec = TargetSpec
    { tsNameVersionLocation :: (Text, Text, SourceLocation) -- ^ the name, version and location of the target package
    , tsTmpDir              :: FilePath                     -- ^ a directory that scoutess will put temporary files in
    , tsLocalHackage        :: LocalHackage                 -- ^ the local hackage repo
    , tsPackageDB           :: FilePath                     -- ^ the directory to be used as a package-db
    , tsSourceConfig        :: SourceConfig                 -- ^ the directory to unpack things from repos in
    }
    deriving Show

data BuildReport = BuildReport
    {
    }
    deriving Show

data PriorRun = PriorRun
    {
    }
    deriving Show

data DependencyGraph = DependencyGraph
    { graph       :: Graph
    , association :: Bimap Vertex VersionInfo
    } deriving Show

deriving instance Typeable2 Bimap
-- | Given that a 'Bimap' is just two 'Map's, this defintion is very similar to the one for 'Map'
instance (Data a, Data b, Ord a, Ord b) => Data (Bimap a b) where
    gfoldl     f z m = z B.fromList `f` B.toList m
    toConstr   _     = error "toConstr"
    gunfold    _ _   = error "gunfold"
    dataTypeOf _     = mkNoRepType "Data.Bimap.Bimap"
    dataCast2  f     = gcast2 f

data LocalHackageIndex = LocalHackageIndex
    { pathToIndex :: FilePath
    }
    deriving Show

----------------------
-- Helper functions --
----------------------

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.).(.)
infixr 9 .:


viName :: VersionInfo -> String
viName = (\(PackageName n) -> n) . pkgName . package . viPackageDescription

viVersion :: VersionInfo -> Version
viVersion = pkgVersion . package . viPackageDescription

viDependencies :: VersionInfo -> [Dependency]
viDependencies = buildDepends . viPackageDescription

srcName :: SourceInfo -> String
srcName = viName . srcVersionInfo

srcVersion :: SourceInfo -> Version
srcVersion = viVersion . srcVersionInfo

srcDependencies :: SourceInfo -> [Dependency]
srcDependencies = viDependencies . srcVersionInfo

-- | Finds a 'VersionInfo' for a package from a 'VersionSpec'

findVersion :: Text           -- ^ package name
            -> Text           -- ^ package version
            -> SourceLocation -- ^ package location
            -> VersionSpec    -- ^ VersionSpec to search in
            -> Maybe VersionInfo
findVersion name version location = (fst <$>) . S.maxView . S.filter isTarget . versions
    where
    isTarget vi = T.unpack name == viName vi &&
                  T.unpack version == showVersion (viVersion vi) &&
                  location == viSourceLocation vi

createVersionInfo :: SourceLocation -> GenericPackageDescription -> VersionInfo
createVersionInfo sourceLocation gpd = versionInfo
    where
    name               = viName versionInfo
    version            = showVersion (viVersion versionInfo)
    versionInfo        = VersionInfo
        { viPackageDescription = flattenPackageDescription gpd
        , viVersionTag         = T.pack (name ++ "-" ++ version)
        , viSourceLocation     = sourceLocation}

-- | looks for all .cabal files in the provided directory and its subdirectories
findCabalFiles :: FilePath      -- ^ where to start looking (recursively)
               -> IO [FilePath] -- ^ the file paths to the .cabal files
findCabalFiles = find recPred (extension ==? ".cabal")
  where recPred = (`notElem` ["_darcs", ".git", "src", "tests", "test", "examples", "Data", "Control", "data"]) <$> fileName

-- | looks for a single .cabal file in the given directory and returns its filepath
findCabalFile :: FilePath -> IO (Maybe FilePath)
findCabalFile fp = do
    cabals <- find (depth ==? 0) (extension ==? ".cabal") fp
    return $ case cabals of
        []  -> Nothing
        [x] -> Just x
        _   -> Nothing
