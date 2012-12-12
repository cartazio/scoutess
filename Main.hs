{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow       (arr)
import Control.Applicative ((<$>))
import Control.Category    (id, (.))
import Data.Monoid         ((<>))
import Data.Set            (fromList)
import Data.Text           (Text, pack, unpack)
import System.Environment  (getArgs, getProgName)
import System.FilePath     ((</>))
import System.Directory    (canonicalizePath)

import Prelude             hiding (id, (.))

import Scoutess.Core
import Scoutess.DataFlow
import Scoutess.Types

-- TODO: we need a real source of options and better (any) support for command line flags.
main :: IO ()
main = getArgs >>= \args -> case args of
    [tName, tVersion, tLocation, sandboxDir, sources]
        -> do sandboxAbs <- canonicalizePath sandboxDir
              result <- buildScoutess tName tVersion tLocation sandboxAbs sources
              putStrLn (ppScoutessResult result)
    _   -> printUsage

buildScoutess :: String -> String -> String -> String -> String -> IO (Maybe BuildReport, [ComponentReport])
buildScoutess tName tVersion tLocation sandboxDir sources = do
        sourceSpec <- SourceSpec . fromList . map read . lines <$> readFile sources
        let targetSpec = makeTargetSpec (pack tName) (pack tVersion) (read tLocation) sandboxDir
        runScoutess
            (standard id id)
            (sourceSpec, targetSpec, Nothing)

onlyAllowHackage :: Scoutess SourceSpec SourceSpec
onlyAllowHackage = arr (filterSourceSpec (== Hackage))

printUsage :: IO ()
printUsage = getProgName >>= \progName -> putStrLn . unlines $
    [ "Usage: " <> progName <> " tName tVersion tLocation sandboxDir sources\n"
    , "    tName:      name of the target package"
    , "    tVersion:   version of the target package"
    , "    tLocation:  SourceLocation of the target package"
    , "    sandboxDir: directory to use as a sandbox"
    , "    sources:    a file containing the accessable"
    , "                SourceLocations seperated by newlines"
    , "For details of the SourceLocations, see Scoutess.Core.SourceLocation.\n"
    , "Example:"
    , "    scoutess vector-algorithms 0.5.4 Hackage ./path/to/sandbox ./path/to/sources.txt\n"
    , "    where sources.txt contains the text \"Hackage\"."
    , "    would install vector-algorithms-0.5.4 in sandbox fetching the package and its dependencies from Hackage.\n"
    , "By default the dependencies can only come from Hackage, to override this behaviour you need to call Scoutess.DataFlow.standard yourself."]

makeTargetSpec :: Text -> Text -> SourceLocation -> FilePath -> TargetSpec
makeTargetSpec name version location sandboxDir = TargetSpec
    { tsName            = name
    , tsVersion         = version
    , tsLocation        = location
    , tsTmpDir          = sandboxDir </> "temp"
    , tsLocalHackage    = LocalHackage (sandboxDir </> "localHackage") (sandboxDir </> "localHackage" </> "temp")
    , tsPackageDB       = sandboxDir </> "package-cache"
    , tsSourceConfig    = SourceConfig (sandboxDir </> "srcCacheDir")
    , tsCustomCabalArgs = ["--enable-documentation"
                          ,"--docdir=" <> pack (sandboxDir </> "docs" </> "$pkg" </> "$version")]
    }
