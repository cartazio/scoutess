-- | 'dataFlow sourceDir sandboxDir' runs Scoutess.
--   The package to build and its cabal file needs to be in sourceDir
--   Note that currently some temporary files are left behind in sandboxDir
--   that must be deleted before running it again.

import Control.Arrow       (arr)
import Control.Applicative ((<$>))
import Data.Set            (singleton, fromList)
import Data.Text           (Text, pack)
import System.Environment  (getArgs)
import System.FilePath     ((</>))

import Scoutess.Core
import Scoutess.DataFlow

main :: IO ()
main = do
    args <- getArgs
    case args of
        [name, version, locationStr, sandboxDir, sourceLocationFile] -> do
                sourceSpec <- SourceSpec . fromList . map read . lines <$> readFile sourceLocationFile
                let targetSpec = makeTargetSpec (pack name) (pack version) (read locationStr) sandboxDir
                runScoutess (standard allowAll allowAll)
                            (sourceSpec, targetSpec, Nothing)
                putStrLn "Scoutess finished"
        _ -> printUsage


allowAll :: Scoutess a a
allowAll = arr id

printUsage :: IO ()
printUsage = putStrLn . unlines $
    [ "arguments are: name version location sandboxDir sourceLocationFile"
    , "where name, version and location are the details of the target package,"
    , "sandboxDir is a folder that this program will put everything in and"
    , "sourceLocationFile points to a file containing one SourceLocation per line."]


makeTargetSpec :: Text -> Text -> SourceLocation -> FilePath -> TargetSpec
makeTargetSpec name version location sandboxDir = TargetSpec
    { tsNameVersionLocation = (name, version, location)
    , tsTmpDir         = sandboxDir </> "temp"
    , tsLocalHackage   = LocalHackage (sandboxDir </> "localHackage") (sandboxDir </> "localHackage" </> "temp")
    , tsPackageDB      = sandboxDir </> "package-cache"
    , tsSourceConfig   = SourceConfig (sandboxDir </> "srcCacheDir")
    }
