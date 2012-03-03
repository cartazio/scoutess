{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Scoutess.Service.Builder.Core where

import Control.Monad                 (foldM)
import Data.Data                     (Data, Typeable)
import Data.SafeCopy                 (SafeCopy, base, deriveSafeCopy)
import Distribution.System           (OS)
import Scoutess.Service.Source.Core  (SourceConfig, SourceException, SourceInfo, SourceLocation)
import Scoutess.Service.Source.Fetch (fetchSrcs)

data HackageDB = HackageDB

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
--
-- If some source fails to check out.. should we try to see what else we can still build ? Probably? We don't do that right now.
buildSources :: OS                -- ^ OS to build for
             -> SourceConfig      -- ^ 'SourceConfig' used by 'fetch'
             -> BuildHistory      -- ^ information about previous builds so we can calculate what needs to be done
             -> HackageDB         -- ^ local hackage db
             -> [HackageDB]       -- ^ remote hackagedbs and local package databases that will be used by 'cabal install'
             -> [SourceLocation]  -- ^ location of sources we care about rebuilding
             -> IO BuildReport
buildSources os sourceConfig buildHistory localHackageDB hackageDBs sources =
    do results <- fetchSrcs sourceConfig sources
       case results of
         (errs, sourceInfos) 
             | not (null errs) -> 
                 do return (BuildReport { sourceResults = results 
                                        , diffs = []
                                        })
             | otherwise ->
                 do let -- first find all the source packages that changed
                        srcChanged     = calculateSrcChanged buildHistory sourceInfos
                    -- update the the local hackage db server
                    -- in theory would could just pass in 'srcChanged' instead of all the 'sourceInfo'. But passing all the 'sourceInfo' seems more robust.
                    localHackageDB' <- foldM addToHackageDB localHackageDB sourceInfos
                        -- then, by tracking build dependencies, figure out what packages need to be rebuilt, and in what order.
                        -- It is possible that 
                    let buildGroups = calculateRebuilds os (localHackageDB':hackageDBs) srcChanged sourceInfos
                    return (BuildReport { sourceResults = results
                                        , diffs = []
                                        })
                             
-- | using the 'BuildHistory' figure out which sources have changed since the latest build. 
--
calculateSrcChanged :: BuildHistory -> [SourceInfo] -> [SourceInfo]
calculateSrcChanged buildHistory srcInfos = undefined

-- | calculate what packages to actually rebuild, and in what order/groups.
--
-- We return a list of lists because if sets of packages have no intersections, then they can (in theory) be built in parallel.
--
-- Each '[SourceInfo]'
--
-- We probably want some way to attempt to force a rebuild.
calculateRebuilds :: OS 
                  -> [HackageDB]     -- ^ packages available from remote hackageDBs or already installed on the system
                  -> [SourceInfo]    -- ^ all sources that had changes
                  -> [SourceInfo]    -- ^ all available sources that we are interested in rebuilding
                  -> [[SourceInfo]]  -- ^ packages to build (grouped and ordered)
calculateRebuilds os hackageDBs srcChanged allSrcs  = 
    undefined


addToHackageDB :: HackageDB -> SourceInfo -> IO HackageDB
addToHackageDB hackageDB sourceInfo = undefined