{-# LANGUAGE Arrows, NamedFieldPuns #-}
module Scoutess.DataFlow where

import Control.Applicative ((<$>))
import Control.Arrow
import Control.Monad (filterM, (<=<))
import Control.Monad.State (State, runState, get, put, gets)
import Data.Array (Array, array)
import Data.Bimap (Bimap)-- O(log n) bijection between 'Int's and 'VersionInfo's used in 'calculateDependencies'
import qualified Data.Bimap as B
import Data.Graph (Vertex)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Distribution.Package
import Distribution.Version (withinRange)

import qualified Scoutess.Service.Source.Hackage as H (fetchAllVersions)
import Scoutess.Core
import Scoutess.Service.LocalHackage.Core
import Scoutess.Service.Source.Core (SourceInfo)

-- standard build
standard :: Scoutess SourceSpec SourceSpec    -- ^ sourceFilter
         -> Scoutess VersionSpec VersionSpec  -- ^ versionFilter
         -> Scoutess (SourceSpec, TargetSpec, PriorRun) BuildReport
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
fetchVersionsFrom Hackage = H.fetchAllVersions sourceConfig
          where sourceConfig = undefined
fetchVersionsFrom  _      = undefined

calculateDependencies :: Scoutess (TargetSpec, VersionSpec) DependencyGraph
calculateDependencies = liftScoutess $ \(targetSpec, versionSpec) ->
    let targetVersion  :: VersionInfo
        targetVersion   = undefined -- convert targetSpec into a VersionInfo
        (depMap, bimap) = runState (dependencyMap versionSpec targetVersion) B.empty
        bounds          = (0, B.size bimap -1)
        depArr         :: Array Vertex [Vertex]
        depArr          = array bounds (M.toList depMap)
    in return DependencyGraph {graph = depArr, association = bimap}

-- | Return the immediate dependencies of a given 'VersionInfo'
--   currently takes the highest valid dependency. If a dependency can't be found,
--   it is silently ignored.
getImmDeps :: VersionSpec -> VersionInfo -> [VersionInfo]
getImmDeps (VersionSpec{versions}) (VersionInfo{viDependencies}) = catMaybes (map findDep viDependencies)
    where findDep dep = fst <$> S.maxView (S.filter (fitsDep dep) versions)
          fitsDep :: Dependency -> VersionInfo -> Bool
          fitsDep      (Dependency name range) vi = validName name vi && validVersion range vi
          validName    name                    vi = name == pkgName (viPackageIdentifier vi)
          validVersion range                   vi = withinRange (pkgVersion (viPackageIdentifier vi)) range


-- | Find the index of a 'VersionInfo' (adding it to the 'Bimap' if it isn't found).
getOrAddVersionIndex :: VersionInfo -> State (Bimap Vertex VersionInfo) Vertex
getOrAddVersionIndex version = do
    bimap <- get
    let addNew = do
          let ix = B.size bimap
          put $ B.insert ix version bimap
          return ix
    maybe addNew return (B.lookupR version bimap)

-- | Returns a 'Map' from this 'VersionInfo''s 'Vertex' to the 'Vertex's corresponding to its dependencies
--   while updating the index if needed.
dependencyMap :: VersionSpec -> VersionInfo -> State (Bimap Vertex VersionInfo) (Map Vertex [Vertex])
dependencyMap spec version = do
    let deps = getImmDeps spec version
    deps'    <- filterM (gets . B.notMemberR) deps
    versionI <- getOrAddVersionIndex version
    depsI    <- mapM getOrAddVersionIndex deps
    depsM    <- mapM (dependencyMap spec) deps'
    return $ M.insert versionI depsI (M.unions depsM)

calculateChanges :: Scoutess (TargetSpec, PriorRun, DependencyGraph) BuildSpec
calculateChanges = liftScoutess $ \(targetSpec', proirRun, depGraph) -> do
    let allPackages = S.fromAscList . map fst . B.toAscListR $ association depGraph
    return BuildSpec {
        targetSpec   = targetSpec'
      , newDeps      = allPackages
      , allDeps      = allPackages}
        -- ^ currently will ignore priorRun and build everything
        --   todo: take only from the graph, not the entire bimap

updateLocalHackage :: Scoutess BuildSpec LocalHackageIndex
updateLocalHackage = liftScoutess $ \buildSpec -> do
    mapM (flip addPackage localHackage <=< getSource) (S.toList $ newDeps buildSpec)
    generateIndexSelectively (Just . S.toList $ allDeps buildSpec) localHackage indexPath
    return $ getLocalIndex indexPath
    where localHackage   = undefined
          indexPath      = undefined
          getSource     :: VersionInfo -> IO SourceInfo
          getSource      = undefined
          getLocalIndex :: FilePath -> LocalHackageIndex
          getLocalIndex  = undefined

build :: Scoutess (LocalHackageIndex, BuildSpec) BuildReport
build = undefined