-- | 'dataFlow sourceDir sandboxDir' runs Scoutess.
--   The package to build and its cabal file needs to be in sourceDir
--   Note that currently some temporary files are left behind om sandboxDir
--   that must be deleted before running it again.

import Control.Arrow      (arr)
import Data.Set           (singleton)
import System.Environment (getArgs)
import System.FilePath    ((</>))

import Scoutess.Core
import Scoutess.DataFlow

main :: IO ()
main = do
    [sourceDir, sandboxDir] <- getArgs
    let targetSpec = makeTargetSpec sourceDir sandboxDir
    runScoutess (standard allowAll allowAll)
                (hackage, targetSpec, Nothing)
    putStrLn "Scoutess finished"

allowAll :: Scoutess a a
allowAll = arr id

hackage :: SourceSpec
hackage = SourceSpec (singleton Hackage)

makeTargetSpec :: FilePath -> FilePath -> TargetSpec
makeTargetSpec sourceDir sandboxDir = TargetSpec
    { tsTmpDir       = sandboxDir </> "temp"
    , tsLocalHackage = LocalHackage (sandboxDir </> "localHackage") (sandboxDir </> "localHackage" </> "temp")
    , tsPackageDB    = sandboxDir </> "package-cache"
    , tsSourceDir    = sourceDir
    , tsSourceConfig = SourceConfig (sandboxDir </> "srcCacheDir")
    }
