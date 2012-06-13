{-# LANGUAGE Arrows, GeneralizedNewtypeDeriving #-}
module Scoutess.DataFlow where

import Control.Arrow
import Control.Category
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Distribution.Package
import Prelude hiding ((.), id)
import qualified Prelude
import qualified Scoutess.Service.Source.Hackage as H (fetchAllVersions)

newtype Scoutess a b = Scoutess (Kleisli IO a b)
    deriving (Category, Arrow, ArrowApply, ArrowChoice, ArrowLoop, ArrowPlus, ArrowZero)
liftScoutess = Scoutess . Kleisli

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
    { versions :: Set VersionInfo
    }
    deriving (Show, Eq, Ord)

data VersionInfo = VersionInfo
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
fetchVersions = liftScoutess $ \sourceSpec -> do
    let locations' = S.toList . locations $ sourceSpec
    versions <- liftM unions (mapM fetchVersionsFrom locations')
    return (VersionSpec{versionInfos = versions})

fetchVersionsFrom :: SourceLocation -> IO (Set VersionInfo)
fetchVersionsFrom Hackage = toVersionInfos H.fetchAllVersions sourceConfig
    where toVersionInfos :: Set (Text,Text) -> Set VersionInfo
          -- ^ possibly defined in Scoutess.Service.Source.Hackage?
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
