module Main where

import Distribution.InstalledPackageInfo
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import qualified Distribution.Simple.Haddock (haddock)
import Distribution.Simple.Setup (ConfigFlags(..), HaddockFlags(..), defaultConfigFlags, defaultHaddockFlags)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.Program (defaultProgramConfiguration)
import Distribution.Simple.Haddock
import Distribution.Verbosity
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import System.FilePath
import System.Posix.Directory

main :: IO ()
main =
    do let srcDir   = "/tmp/unpack-dir/happstack-server-6.6.4"
           dotCabal = srcDir </> "happstack-server.cabal"
       pkgDesc <- readPackageDescription silent dotCabal
       lbi <- configure (pkgDesc, emptyHookedBuildInfo) (defaultConfigFlags defaultProgramConfiguration)
--        print lbi
       changeWorkingDirectory srcDir
       haddock (localPkgDescr lbi) (removeHaddock lbi) [] (defaultHaddockFlags { haddockProgramArgs = [("-v",[])] })
       return ()



removeHaddock :: LocalBuildInfo -> LocalBuildInfo
removeHaddock lbi =
    let packages = PackageIndex.allPackages (installedPkgs lbi)
    in  lbi { installedPkgs = PackageIndex.fromList (map removeHaddock' packages) }


removeHaddock' :: InstalledPackageInfo -> InstalledPackageInfo
removeHaddock' ipi =
    ipi { haddockInterfaces = []
        , haddockHTMLs      = []
        }