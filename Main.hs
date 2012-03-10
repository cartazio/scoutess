module Main where

import Distribution.Package
import Distribution.Text             (simpleParse)
import Data.Maybe                    (fromJust)
import Scoutess.Service.Haddock.Core (runHaddock)

main :: IO ()
main = runHaddock "/tmp/unpack-dir" "/tmp/doc-dir" (fromJust $ simpleParse "happstack-server-6.6.4")
