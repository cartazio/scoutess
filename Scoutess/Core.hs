{-# LANGUAGE OverloadedStrings #-}

module Scoutess.Core where

import Control.Arrow
import Control.Applicative          ((<$>))
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.List                    (isPrefixOf)
import qualified Data.Map as M
import Data.Maybe                   (mapMaybe, isJust, fromJust)
import Data.Set                     (Set)
import qualified Data.Set as S
import Data.Text                    (Text)
import qualified Data.Text as T
import Data.Version                 (parseVersion, showVersion)
import System.Directory             (createDirectoryIfMissing)
import System.FilePath              ((</>), (<.>))
import System.FilePath.Find         (find, fileName, extension, (==?), depth)
import Text.ParserCombinators.ReadP

import Prelude hiding ((++))

import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.PrettyPrint (writeGenericPackageDescription)
import Distribution.Version (Version)

import Scoutess.Types
import Scoutess.Utils.Archives

--------------------
-- Scoutess arrow --
--------------------
runScoutess :: Scoutess a b -> a -> IO (Maybe b, [ComponentReport])
runScoutess = runWriterT . runMaybeT .: runKleisli . unScoutess

liftScoutess :: (a -> Scoutess' b) -> Scoutess a b
liftScoutess = Scoutess . Kleisli

----------------
-- Components --
----------------
withComponent :: Text -> (a -> Component b) -> Scoutess a b
withComponent name action = liftScoutess ((extractReportValue =<<) . (lift . lift . runWriterT . runMaybeT . action))
    where
    extractReportValue :: (Maybe (Bool, b), [Text]) -> Scoutess' b
    extractReportValue (Nothing, cLog)               = do
        tell [ComponentReport name False (T.unlines cLog)]
        mzero
    extractReportValue (Just (success, value), cLog) = do
        tell [ComponentReport name success (T.unlines cLog)]
        return value

report :: Text -> Component ()
report text = tell [text] >> componentPass ()

componentPass, componentFail :: a -> Component a
componentPass value = return (True, value)
componentFail value = return (False, value)

componentFinish :: Bool -> a -> Component a
componentFinish True  = componentPass
componentFinish False = componentFail

componentFatal :: Component a
componentFatal = mzero

---------------------
-- Container types --
---------------------
filterSourceSpec :: (SourceLocation -> Bool) -> SourceSpec -> SourceSpec
filterSourceSpec p = SourceSpec . S.filter p . ssLocations

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

-----------
-- Other --
-----------
-- | return a human readable error message
sourceErrorMsg :: SourceException -- ^ error
               -> Text            -- ^ error message
sourceErrorMsg (SourceErrorOther txt) = txt
sourceErrorMsg (SourceErrorUnknown)   = "unknown source error"



toPriorRun :: BuildReport -> PriorRun
toPriorRun br = PriorRun (bsTargetInfo buildSpec) (S.fromList (bsDependencies buildSpec))
    where buildSpec = brBuildSpec br

-- | If the target of the 'PriorRun' is
--   different to that of the 'BuildReport', return Nothing.
--   Otherwise, return a pair the two differences e.g.:
--       "findDifference buildReport priorRun
--           = (deps buildReport \\ deps priorRun, depsPriorRun \\ deps buildReport)"
findDifference :: BuildReport -> PriorRun -> Maybe (Set VersionInfo, Set VersionInfo)
findDifference buildReport priorRun
    | bsTargetInfo buildSpec == prTarget priorRun
                = Just (deps S.\\ deps', deps' S.\\ deps)
    | otherwise = Nothing
    where
    buildSpec = brBuildSpec buildReport
    deps  = S.fromList (bsDependencies buildSpec)
    deps' = prDependencies priorRun

noDifference :: BuildReport -> PriorRun -> Bool
noDifference br pr = case findDifference br pr of
    Just (diff1, diff2) | S.null diff1 && S.null diff2 -> True
    _                                                  -> False

----------------------
-- Helper functions --
----------------------
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
infixr 8 .:

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

(++) :: Monoid xs => xs -> xs -> xs
(++) = mappend
