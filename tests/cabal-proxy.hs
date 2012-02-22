-- built with ghc -i"../" --make cabal-proxy.hs

import Scoutess.Service.CabalProxy.Core
import System.Environment (getArgs)

main = do
  [dir] <- getArgs
  cabals <- findCabalFiles dir
  mapM_ putStrLn cabals
  tarCabalFiles cabals dir "pkgs.tar"