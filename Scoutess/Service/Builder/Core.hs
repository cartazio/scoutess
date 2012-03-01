{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Scoutess.Service.Builder.Core where

import Data.Data   (Data, Typeable)
import Data.Either (partitionEithers)
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)
import Scoutess.Service.Source.Core (SourceConfig, SourceException, SourceInfo, SourceLocation)
import Scoutess.Service.Source.Fetch (fetch)

data BuildHistory = BuildHistory {
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''BuildHistory)

data Diffs = Diffs
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Diffs)

data BuildReport = BuildReport {
      sourceResults :: [Either SourceException SourceInfo]
    , diffs         :: [Diffs] -- ^ would be nice to get some sort of 'diffs' that show exactly what changed in the source between two builds
    }
    deriving (Eq, Read, Show, Typeable)
$(deriveSafeCopy 0 'base ''BuildReport)

-- | attempt to build the specified sources and generate a report
--
-- We probably want to have the option to force rebuilds or just rebuild things which have changes.
--
-- Also, we have a couple module that want to know similar information about changes. For example, this module checks that 'cabal build' works. But we have other modules that want to rebuild haddock documentation, extract diffs/changelogs, etc. So, we probably need to have some common functionality that updates the sources and generates a report of what needs to be done. Then we pass that report to the Builder, Haddocks, Diffs, etc.
buildSources :: SourceConfig      -- ^ 'SourceConfig' used by 'fetch'
             -> BuildHistory      -- ^ information about previous builds so we can calculate what needs to be done
             -> [SourceLocation] 
             -> IO BuildReport
buildSources sourceConfig buildHistory sources =
    do results <- mapM (fetch sourceConfig) sources
       case partitionEithers results of
         (errs, success) 
             | not (null errs) -> return (BuildReport { sourceResults = results 
                                                      , diffs = []
                                                      })
                             

-- buildSource :: SourceLocation -> IO BuildReport
                 
{-
buildPackages :: [PackageIdentifier] -> IO BuildReport
buildPackages packages = mapM buildPackage packages

buildPackage :: PackageIdentifier -> IO BuildReporte
buildPackage packageIdentifier = undefined
-}