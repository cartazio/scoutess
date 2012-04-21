module Main where

import Distribution.Package
import Distribution.Text             (simpleParse)
import Data.Maybe                    (fromJust)
import Scoutess.Service.Haddock.Core (runHaddock)

main :: IO ()
main =
    do runHaddock "/tmp/unpack-dir" "/tmp/doc-dir" (fromJust $ simpleParse "happstack-server-7.0.1")
       runHaddock "/tmp/unpack-dir" "/tmp/doc-dir" (fromJust $ simpleParse "happstack-server-tls-7.0.0")
       runHaddock "/tmp/unpack-dir" "/tmp/doc-dir" (fromJust $ simpleParse "happstack-hsp-7.0.2")
       runHaddock "/tmp/unpack-dir" "/tmp/doc-dir" (fromJust $ simpleParse "acid-state-0.6.3")
       runHaddock "/tmp/unpack-dir" "/tmp/doc-dir" (fromJust $ simpleParse "hsx-jmacro-7.0.1")
       runHaddock "/tmp/unpack-dir" "/tmp/doc-dir" (fromJust $ simpleParse "happstack-jmacro-7.0.1")
       runHaddock "/tmp/unpack-dir" "/tmp/doc-dir" (fromJust $ simpleParse "happstack-heist-7.0.0")
       runHaddock "/tmp/unpack-dir" "/tmp/doc-dir" (fromJust $ simpleParse "happstack-7.0.0")
       runHaddock "/tmp/unpack-dir" "/tmp/doc-dir" (fromJust $ simpleParse "happstack-hamlet-7.0.1")
       runHaddock "/tmp/unpack-dir" "/tmp/doc-dir" (fromJust $ simpleParse "happstack-lite-7.1.1")
       runHaddock "/tmp/unpack-dir" "/tmp/doc-dir" (fromJust $ simpleParse "happstack-hstringtemplate-7.0.1")
       runHaddock "/tmp/unpack-dir" "/tmp/doc-dir" (fromJust $ simpleParse "happstack-clientsession-7.0.0")
--       runHaddock "/tmp/unpack-dir" "/tmp/doc-dir" (fromJust $ simpleParse "happstack-plugins-7.0.1")
       runHaddock "/tmp/unpack-dir" "/tmp/doc-dir" (fromJust $ simpleParse "ixset-1.0.3")
       runHaddock "/tmp/unpack-dir" "/tmp/doc-dir" (fromJust $ simpleParse "web-routes-0.27.1")
       runHaddock "/tmp/unpack-dir" "/tmp/doc-dir" (fromJust $ simpleParse "web-routes-boomerang-0.26.0")
       runHaddock "/tmp/unpack-dir" "/tmp/doc-dir" (fromJust $ simpleParse "web-routes-happstack-0.23.3")
       runHaddock "/tmp/unpack-dir" "/tmp/doc-dir" (fromJust $ simpleParse "web-routes-hsp-0.22.1")
       runHaddock "/tmp/unpack-dir" "/tmp/doc-dir" (fromJust $ simpleParse "web-routes-th-0.21.1")

