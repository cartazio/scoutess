{-# LANGUAGE Arrows #-}
module Scoutes.DataFlow where

import Control.Arrow
import Control.Category
import Control.Monad
import Data.Set (Set)
import Prelude hiding ((.), id)
import qualified Prelude

data Scoutess a b = Scoutess (a -> IO b)

instance Category Scoutess where
    id  = Scoutess $ return
    (Scoutess f) . (Scoutess g) = Scoutess $ f <=< g

instance Arrow Scoutess where
    arr f = Scoutess (return . f)
    first (Scoutess f) =
        Scoutess $ \(b,d) ->
            do c <- f b
               return (c, d)

    second (Scoutess f) =
        Scoutess $ \(d, b) ->
            do c <- f b
               return (d, c)

data SourceSpec
    = SourceSpec { locations :: Set SourceLocation }
      deriving Show

data SourceLocation
    = Darcs
    | LocalHackage
    | AlreadyGot
      deriving Show

data TargetSpec = TargetSpec
    {
    }
    deriving Show

data BuildReport = BuildReport
    {
    }
    deriving Show

data VersionSpec = VersionSpec
    {
    }
    deriving Show

data PriorRun = PriorRun
    {
    }
    deriving Show

data BuildSpec = BuildSpec
    {
    }
    deriving Show

data DependencyGraph = DependencyGraph
    {
    }
    deriving Show

data LocalHackageIndex = LocalHackageIndex
    {
    }
    deriving Show


-- standard build
standard :: Scoutess SourceSpec SourceSpec -> Scoutess VersionSpec VersionSpec -> Scoutess (SourceSpec, TargetSpec, PriorRun) BuildReport
standard sourceFilter versionFilter =
    proc (sourceSpec, targetSpec, priorRun) ->
        do availableVersions  <- fetchVersions <<< sourceFilter -< sourceSpec
           consideredVersions <- versionFilter                  -< availableVersions
           dependencyGraph    <- calculateDependencies          -< (targetSpec, consideredVersions)
           buildSpec          <- calculateChanges               -< (targetSpec, priorRun, dependencyGraph)
           hackageIndex       <- updateLocalHackage             -< buildSpec
           buildReport        <- build                          -< (hackageIndex, buildSpec)
           returnA -< buildReport

fetchVersions :: Scoutess SourceSpec VersionSpec
fetchVersions = undefined

calculateDependencies :: Scoutess (TargetSpec, VersionSpec) DependencyGraph
calculateDependencies = undefined

calculateChanges :: Scoutess (TargetSpec, PriorRun, DependencyGraph) BuildSpec
calculateChanges = undefined

updateLocalHackage :: Scoutess BuildSpec LocalHackageIndex
updateLocalHackage = undefined

build :: Scoutess (LocalHackageIndex, BuildSpec) BuildReport
build = undefined
