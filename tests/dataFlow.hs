import Control.Arrow       (arr)
import Control.Applicative ((<$>))
import Control.Category    (id, (.))
import Data.Set            (fromList)
import Data.Text           (Text, pack)
import System.Environment  (getArgs)
import System.FilePath     ((</>))

import Prelude hiding (filter, id, (.))

import Scoutess.Core
import Scoutess.DataFlow

main :: IO ()
main = getArgs >>= buildScoutess >> return ()

buildScoutess :: [String] -> IO ()
buildScoutess [name, version, locationStr, sandboxDir, sourceLocationFile] = do
        sourceSpec <- SourceSpec . fromList . map read . lines <$> readFile sourceLocationFile
        let targetSpec = makeTargetSpec (pack name) (pack version) (read locationStr) sandboxDir
        runScoutess
            (standard onlyAllowHackage id)
            (sourceSpec, targetSpec, Nothing)
        return ()
buildScoutess _ = putStrLn usage

onlyAllowHackage :: Scoutess SourceSpec SourceSpec
onlyAllowHackage = arr (filterSourceSpec (== Hackage))

usage :: String
usage = unlines $
    [ "arguments are: name version location sandboxDir sourceLocationFile"
    , "where name, version and location are the details of the target package,"
    , "sandboxDir is a folder that this program will put everything in and"
    , "sourceLocationFile points to a file containing one SourceLocation per line."]


makeTargetSpec :: Text -> Text -> SourceLocation -> FilePath -> TargetSpec
makeTargetSpec name version location sandboxDir = TargetSpec
    { tsName            = name
    , tsVersion         = version
    , tsLocation        = location
    , tsTmpDir          = sandboxDir </> "temp"
    , tsLocalHackage    = LocalHackage (sandboxDir </> "localHackage") (sandboxDir </> "localHackage" </> "temp")
    , tsPackageDB       = sandboxDir </> "package-cache"
    , tsSourceConfig    = SourceConfig (sandboxDir </> "srcCacheDir")
    , tsCustomCabalArgs = []
    }
