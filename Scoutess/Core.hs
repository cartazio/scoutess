{-# LANGUAGE GeneralizedNewtypeDeriving
           , DeriveDataTypeable
           , OverloadedStrings
           , StandaloneDeriving #-}

module Scoutess.Core where

import Control.Arrow
import Control.Applicative  ((<$>))
import Control.Category     (Category)
import Control.Exception    (Exception)
import Control.Monad        (guard)
import Control.Monad.Error  (Error(..))
import Data.Data            (Typeable, Typeable2, Data(..), mkNoRepType, gcast2)
import Data.List            (isPrefixOf)
import qualified Data.Map as M
import Data.Maybe           (mapMaybe, isJust, fromJust)
import Data.Monoid
import Data.Set             (Set)
import qualified Data.Set as S
import Data.Text            (Text)
import qualified Data.Text as T
import Data.Version         (parseVersion, showVersion)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(..))
import System.FilePath      ((</>), (<.>))
import System.FilePath.Find (find, fileName, extension, (==?), depth)
import Text.ParserCombinators.ReadP

import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.PrettyPrint (writeGenericPackageDescription)
import Distribution.Version (Version)

import Scoutess.Utils.Archives


--------------------
-- Scoutess arrow --
--------------------

newtype Scoutess a b = Scoutess {unScoutess :: Kleisli IO a b}
    deriving (Category, Arrow, ArrowApply, ArrowChoice, ArrowLoop, ArrowPlus, ArrowZero)

runScoutess :: Scoutess a b -> a -> IO b
runScoutess = runKleisli . unScoutess

liftScoutess :: (a -> IO b) -> Scoutess a b
liftScoutess = Scoutess . Kleisli

---------------------------
-- Dealing with packages --
---------------------------

-- | Information about a package that we've seen the cabal file of but not neccesarily fetched yet
data VersionInfo = VersionInfo
    { viGPD            :: GenericPackageDescription
    , viVersionTag     :: Text
      -- ^ a unique version identifier.
      --   For packages from Hackage, this is equal to "<name>-<version>".
      --   For packages from source control systems, this contains information about the latest patch.
    , viSourceLocation :: SourceLocation
    } deriving Show

-- | Information about a package which has been fetched and is locally available now
data SourceInfo = SourceInfo
    { siPath         :: FilePath           -- ^ path to the directory that contains the .cabal file
    , siVersionInfo  :: VersionInfo        -- ^ information about the source
    }
    deriving (Show, Typeable, Eq, Ord)

-- | 'Eq' and 'Ord' will ignore the 'GenericPackageDescription'. 'Ord' because it isn't an instance
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
    = SourceSpec { ssLocations :: Set SourceLocation }
      deriving Show

filterSourceSpec :: (SourceLocation -> Bool) -> SourceSpec -> SourceSpec
filterSourceSpec p = SourceSpec . S.filter p . ssLocations

-- | Corresponds to a virtual repository
data VersionSpec = VersionSpec
    { vsVersions    :: Set VersionInfo -- The packages contained in the repository
    , vsPreferences :: Maybe Text      -- ^ Repositories can also contain a file specifying preferred versions.
    }
    deriving (Show, Eq, Ord)

instance Monoid VersionSpec where
    mappend vs1 vs2 = VersionSpec (vsVersions vs1 `mappend` vsVersions vs2) (vsPreferences vs1 `mappend` vsPreferences vs2)
    mempty = VersionSpec mempty mempty
    mconcat vss = VersionSpec (mconcat (map vsVersions vss)) (mconcat (map vsPreferences vss))

filterVersionSpec :: (VersionInfo -> Bool) -> VersionSpec -> VersionSpec
filterVersionSpec p vs = VersionSpec (S.filter p (vsVersions vs)) (vsPreferences vs)

-- TODO: write out the versionPreferences
createPackageIndexWith :: (VersionInfo -> VersionInfo -> VersionInfo) -> VersionSpec -> FilePath -> IO FilePath
createPackageIndexWith conflict vs baseDir = do
    mapM_ (\((name, version), vi) -> writeCabal (baseDir </> name </> version) vi) versions'
    tarFiles (map (\((name, version),_) -> name </> version) versions') baseDir archiveLoc
    return archiveLoc
    where
    archiveLoc = baseDir </> "00-index.tar"
    versions' = M.toList $ M.fromListWith conflict [((viName vi, showVersion (viVersion vi)), vi) | vi <- S.toList (vsVersions vs)]

writeCabal :: FilePath -> VersionInfo -> IO FilePath
writeCabal dir versionInfo = do
    createDirectoryIfMissing True dir
    writeGenericPackageDescription dir' (viGPD versionInfo)
    return dir'
    where
    name    = viName versionInfo
    dir'    = dir </> name <.> ".cabal"

findIn :: [PackageIdentifier] -> Set VersionInfo -> [VersionInfo]
findIn pkgIdens vis = mapMaybe (\pkgIden -> fst <$> S.maxView (S.filter ((pkgIden ==) . viPackageIden) vis)) pkgIdens

data BuildSources = BuildSources
    { targetSource :: SourceInfo   -- ^ The source for the target
    , depSources   :: [SourceInfo] -- ^ The sources for the dependencies (in reverse topological order)
    }
    deriving Show

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

data BuildSpec = BuildSpec
    { bsTargetSpec    :: TargetSpec     -- ^ input to scoutess
    , bsTargetInfo    :: VersionInfo    -- ^ information about the target package
    , bsDependencies  :: [VersionInfo]  -- ^ in reverse topological order
    , bsPriorRun      :: Maybe PriorRun -- ^ information from the previous run
    , bsToBuild       :: Bool           -- ^ is there a build needed?
    }
    deriving Show

-- | The initial input to scoutess
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

-- | Any errors or exceptions encountered when fetching the versions or the sources
data RecordedExceptions = RecordedExceptions
    { versionExceptions :: Set SourceException
    , sourceExceptions  :: Maybe (Set SourceException)
    }
    deriving (Eq, Ord, Show, Read)

-- TODO: should we just have a link to the LocalBuildInfo?
data BuildReport = BuildReport
    { brBuildSpec    :: BuildSpec
    , brExceptions   :: RecordedExceptions -- ^ any errors produced when fetching the versions or sources.
    , brCabalResults :: Maybe (ExitCode, Text, Text, Text)
        -- ^ The exit code, standard out, standard error and the contents of the log file produced by calling cabal
    , brBuildSources :: Maybe BuildSources -- ^ the sources that we built (if we built)
    }
    deriving Show

brSucceeded :: BuildReport -> Bool
brSucceeded br = maybe False wasSuccess (brCabalResults br) && isJust (brBuildSources br)
    where
    wasSuccess :: (ExitCode, Text, Text, Text) -> Bool
    wasSuccess (ExitSuccess, _, _, _) = True
    wasSuccess _                      = False

data PriorRun = PriorRun
    { prTarget       :: VersionInfo
    , prDependencies :: Set VersionInfo
    }
    deriving Show

toPriorRun :: BuildReport -> PriorRun
toPriorRun br = PriorRun (bsTargetInfo buildSpec) (S.fromList (bsDependencies buildSpec))
    where buildSpec = brBuildSpec br

-- | if there is no prior run or the target of the prior run is
--   different to that of the build report, return Nothing.
--   Otherwise, return the difference between the dependencies
--   XXX: not completed
findDifference :: BuildReport -> Maybe (Set VersionInfo)
findDifference br = undefined

data LocalHackageIndex = LocalHackageIndex
    { pathToIndex :: FilePath
    }
    deriving Show

----------------------
-- Helper functions --
----------------------
viName :: VersionInfo -> String
viName = (\(PackageName n) -> n) . pkgName . viPackageIden

viVersion :: VersionInfo -> Version
viVersion = pkgVersion . viPackageIden

viPackageIden :: VersionInfo -> PackageIdentifier
viPackageIden = package . packageDescription . viGPD

viDependencies :: VersionInfo -> [Dependency]
viDependencies = buildDepends . packageDescription . viGPD

srcName :: SourceInfo -> String
srcName = viName . siVersionInfo

srcVersion :: SourceInfo -> Version
srcVersion = viVersion . siVersionInfo

srcDependencies :: SourceInfo -> [Dependency]
srcDependencies = viDependencies . siVersionInfo

-- | Finds a 'VersionInfo' for a package from a 'VersionSpec'

findVersion :: Text           -- ^ package name
            -> Text           -- ^ package version
            -> SourceLocation -- ^ package location
            -> VersionSpec    -- ^ VersionSpec to search in
            -> Maybe VersionInfo
findVersion name version location vs = (fst <$>) . S.maxView . S.filter isTarget . vsVersions $ vs
    where
    isTarget vi =
        T.unpack name == viName vi &&
        T.unpack version == showVersion (viVersion vi) &&
        location == viSourceLocation vi

createVersionInfo :: SourceLocation -> GenericPackageDescription -> VersionInfo
createVersionInfo sourceLocation gpd = versionInfo
    where
    name               = viName versionInfo
    version            = showVersion (viVersion versionInfo)
    versionInfo        = VersionInfo
        { viGPD            = gpd
        , viVersionTag     = T.pack (name ++ "-" ++ version)
        , viSourceLocation = sourceLocation}

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

-- | parse the output of cabal-install --dry-run
--   TODO: check what error message we get if cabal can't find the dependencies
parseDependencies :: String -> [PackageIdentifier]
-- look for the line "In order, the following would be installed"
-- then parse until we can't find a valid name-version
parseDependencies = safeInit . map fromJust . takeWhile isJust
                  . map parsePackageId . drop 1
                  . dropWhile (not . isPrefixOf "In order, the following") . lines
    where
    safeInit [] = []
    safeInit xs = init xs
    parsePackageId :: String -> Maybe PackageIdentifier
    parsePackageId = uncurry toPackageIden . first (T.dropWhileEnd (=='-')) . T.breakOnEnd "-" . T.pack . takeWhile (/= ' ')
    -- | parses text of the form "package-name-1.2.3.4"
    toPackageIden :: Text -> Text -> Maybe PackageIdentifier
    toPackageIden name version = do
        let name'    = PackageName (T.unpack name)
            versions = readP_to_S parseVersion (T.unpack version)
        guard . not . null $ versions
        Just . PackageIdentifier name' . fst . last $ versions
