module Main where

import Distribution.Package
import Distribution.Text
import Data.Maybe (fromJust)
import Scoutess.Service.Haddock.Core (haddock)

main :: IO ()
main = haddock "/tmp/unpack-dir" "/tmp/doc-dir" (fromJust $ simpleParse "happstack-server-6.6.4")
