{-# LANGUAGE OverloadedStrings #-}

-- | 'fetch-hackage pkg' fetches and unpacks the sources of the given package
--   and prints some information about it

import Scoutess.Service.Source.Core
import Scoutess.Service.Source.Hackage (fetchHackage)

import Data.Text                       (Text, pack, unpack)
import Distribution.Package
import Distribution.PackageDescription
import System.Environment              (getArgs)

main = do
  [pkg] <- getArgs
  let dir = "./"
  result <- fetchHackage (SourceConfig dir) (pack pkg) Nothing
  case result of 
    Left except      -> putStrLn (show except)
    Right sourceInfo -> printPackageInfo sourceInfo
    
  where printPackageInfo si = do
          let pkgDescr      = srcPackageDescription si
          let pkgIdentifier = package pkgDescr 
          putStrLn $ "Fetched package " ++ unpack (srcVersion si)
          putStrLn $ " maintained by " ++ maintainer pkgDescr
          putStrLn $ " licensed under " ++ show (license pkgDescr)
          putStrLn $ " with sources living in " ++ srcPath si