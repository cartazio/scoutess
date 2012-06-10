module Main where

import Distribution.Package
import Distribution.Text             (simpleParse)
import Data.Maybe                    (fromJust)
import Scoutess.Service.Haddock.Core (runHaddock)

haddockPackage :: String -> IO ()
haddockPackage ident =
    runHaddock "/home/stepcut/n-heptane/projects/haskell/scoutess/_tmp/unpack-dir" "/home/stepcut/n-heptane/projects/haskell/scoutess/_tmp/doc-dir" (fromJust $ simpleParse ident)

main :: IO ()
main =
    let packages =
            [ "acid-state-0.6.3"
            , "happstack-server-7.0.2"
            , "happstack-server-tls-7.0.0"
            , "happstack-hsp-7.1.0"
            , "hsx-jmacro-7.1.1"
            , "happstack-jmacro-7.0.1"
            , "happstack-heist-7.0.0"
            , "happstack-7.0.0"
            , "happstack-hamlet-7.0.1"
            , "happstack-lite-7.2.0"
            , "happstack-hstringtemplate-7.0.1"
            , "happstack-clientsession-7.1.0"
            , "happstack-yui-7351.4.1"
            -- , "happstack-plugins-7.0.1")
            , "reform-0.1.1"
            , "reform-blaze-0.1"
            , "reform-happstack-0.1.1"
            , "reform-hsp-0.1.1"
            , "ixset-1.0.3"
            , "web-routes-0.27.1"
            , "web-routes-boomerang-0.26.0"
            , "web-routes-happstack-0.23.3"
            , "web-routes-hsp-0.23.0"
            , "web-routes-th-0.21.1"
            ]
       in mapM_ haddockPackage packages

