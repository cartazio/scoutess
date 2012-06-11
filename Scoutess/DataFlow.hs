{-# LANGUAGE Arrows #-}
module Scoutess.DataFlow where

import Control.Arrow
import Control.Category
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Prelude hiding ((.), id)
import qualified Prelude
import qualified Scoutess.Service.Source.Hackage as H (fetchAllVersions)

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

instance ArrowChoice Scoutess where
        left f = f +++ arr id
        right f = arr id +++ f
        f +++ g = (f >>> arr Left) ||| (g >>> arr Right)
        Scoutess f ||| Scoutess g = Scoutess (either f g)

data SourceSpec
    = SourceSpec { locations :: Set SourceLocation }
      deriving Show

data SourceLocation
    = Darcs
    | LocalHackage
    | AlreadyGot
    | Hackage
      deriving (Show, Eq, Ord)

data TargetSpec = TargetSpec
    {
    }
    deriving Show

data BuildReport = BuildReport
    {
    }
    deriving Show

data VersionSpec = VersionSpec
    { versionInfos :: Set VersionInfo
    }
    deriving (Show, Eq, Ord)

data VersionInfo = VersionInfo
    {
    }
    deriving (Show, Eq, Ord)

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
fetchVersions = Scoutess $ \sourceSpec -> do
    let locations' = S.toList . locations $ sourceSpec
    versions <- liftM concatSets (mapM fetchVersionsFrom locations')
    return (VersionSpec{versionInfos = versions})
    where concatSets :: Ord a => [Set a] -> Set a
          concatSets = foldl S.union S.empty

fetchVersionsFrom :: SourceLocation -> IO (Set VersionInfo)
fetchVersionsFrom Hackage = toVersionInfos =<< H.fetchAllVersions sourceConfig
    where toVersionInfos :: Set (Text,Text) -> IO (Set VersionInfo)
          -- ^ this is just temporary, it should be defined in Scoutess.Service.Source.Hackage
          toVersionInfos  = undefined
          sourceConfig    = undefined
fetchVersionsFrom  _      = undefined

calculateDependencies :: Scoutess (TargetSpec, VersionSpec) DependencyGraph
calculateDependencies = undefined

calculateChanges :: Scoutess (TargetSpec, PriorRun, DependencyGraph) BuildSpec
calculateChanges = undefined

updateLocalHackage :: Scoutess BuildSpec LocalHackageIndex
updateLocalHackage = undefined

build :: Scoutess (LocalHackageIndex, BuildSpec) BuildReport
build = undefined

