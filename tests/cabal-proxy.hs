-- built with ghc -i"../" --make cabal-proxy.hs

-- | Find cabal files in the given directory and its subdirs and
--   generates a (cabal/hackage) package index from them

import Scoutess.Service.CabalProxy.Core
import System.Environment (getArgs)

main = do
  [dir] <- getArgs
  cabals <- findCabalFiles dir
  mapM_ putStrLn cabals
  tarCabalFiles cabals dir "pkgs.tar"