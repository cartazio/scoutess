{-# LANGUAGE Arrows, GeneralizedNewtypeDeriving #-}
module Scoutess.DataFlow where

import Control.Arrow
import Control.Category
import Control.Monad (liftM)
import Control.Monad.State (State, runState, get, put, execState)
import Data.Array (Array, array)
import Data.Bimap (Bimap)-- O(log n) bijection between 'Int's and 'VersionInfo's used in 'calculateDependencies'
import qualified Data.Bimap as B
import Data.Graph (Graph, Vertex)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Distribution.Package
import Prelude hiding ((.), id)
import qualified Prelude

import qualified Scoutess.Service.Source.Hackage as H (fetchAllVersions)

newtype Scoutess a b = Scoutess (Kleisli IO a b)
    deriving (Category, Arrow, ArrowApply, ArrowChoice, ArrowLoop, ArrowPlus, ArrowZero)

liftScoutess :: (a -> IO b) -> Scoutess a b
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
    { graph :: Graph
    , association :: Bimap Vertex VersionInfo
    }

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
    let locations' = S.toList (locations sourceSpec)
    versionsL <- mapM fetchVersionsFrom locations'
    return VersionSpec{versions = S.unions versionsL}

fetchVersionsFrom :: SourceLocation -> IO (Set VersionInfo)
fetchVersionsFrom Hackage = toVersionInfos `liftM` H.fetchAllVersions sourceConfig
    where toVersionInfos :: Set (Text,Text) -> Set VersionInfo
          -- ^ possibly defined in Scoutess.Service.Source.Hackage?
          toVersionInfos  = undefined
          sourceConfig    = undefined
fetchVersionsFrom  _      = undefined

calculateDependencies :: Scoutess (TargetSpec, VersionSpec) DependencyGraph
calculateDependencies = liftScoutess $ \(targetSpec, versionSpec) ->
    let targetVersion :: VersionInfo
        targetVersion = undefined -- convert targetSpec into a VersionInfo

        depMap :: Set (VersionInfo, (Set VersionInfo))
        depMap = dependencyMap versionSpec targetVersion

        -- change the container types and transform all of the 'VersionInfo's
        -- into the corresponding 'Vertex'
        toVersionInfos (v,deps) = do
            v'    <- getVersionIndex v
            deps' <- mapM getVersionIndex (S.toList deps)
            return (v',deps')
        (vertexList, bimap) = runState (mapM toVersionInfos (S.toList depMap)) B.empty
        bounds = (0, B.size bimap -1)
        depArr :: Array Vertex [Vertex]
        depArr = array bounds vertexList
    in return DependencyGraph {graph = depArr, association = bimap}

-- | Return the immediate dependencies of a given 'VersionInfo'
getImmDeps :: VersionSpec -> VersionInfo -> Set VersionInfo
getImmDeps  = undefined

-- | Find the index of a VersionInfo (adding it to the 'Bimap' if it isn't found).
getVersionIndex :: VersionInfo -> State (Bimap Int VersionInfo) Int
getVersionIndex version = do
    bimap <- get
    let addNew = do
        let ix = B.size bimap
        put $ B.insert ix version bimap
        return ix
    maybe addNew return (B.lookupR version bimap)

-- | create a 'Set' consisting of all the required 'VersionInfo's and each of their dependencies
dependencyMap :: VersionSpec -> VersionInfo -> Set (VersionInfo, (Set VersionInfo))
dependencyMap spec version = execState (dependencyMap' spec version) S.empty
    where
    dependencyMap' :: VersionSpec -> VersionInfo -> State (Set (VersionInfo, (Set VersionInfo))) ()
    dependencyMap' spec version = do
        depMap <- get
        let deps    = getImmDeps spec version
            newDeps = deps S.\\ (S.map fst depMap) -- get rid of dependencies we've already seen
            depMap' = S.insert (version, newDeps) depMap
        put depMap'
        mapM_ (dependencyMap' spec) (S.toList newDeps)

calculateChanges :: Scoutess (TargetSpec, PriorRun, DependencyGraph) BuildSpec
calculateChanges = undefined

updateLocalHackage :: Scoutess BuildSpec LocalHackageIndex
updateLocalHackage = undefined

build :: Scoutess (LocalHackageIndex, BuildSpec) BuildReport
build = undefined
