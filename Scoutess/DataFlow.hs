{-# LANGUAGE Arrows, GeneralizedNewtypeDeriving, StandaloneDeriving, DeriveDataTypeable, NamedFieldPuns #-}
module Scoutess.DataFlow where

import Control.Applicative ((<$>))
import Control.Arrow
import Control.Category (Category)
import Control.Monad (filterM)
import Control.Monad.State (State, runState, get, put, gets)
import Data.Array (Array, array)
import Data.Bimap (Bimap)-- O(log n) bijection between 'Int's and 'VersionInfo's used in 'calculateDependencies'
import qualified Data.Bimap as B
import Data.Data (Typeable2, Data(..), mkNoRepType, gcast2)
import Data.Graph (Graph, Vertex)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Distribution.Package
import Distribution.Version (VersionRange(..), withinRange)

import qualified Scoutess.Service.Source.Hackage as H (fetchAllVersions)

newtype Scoutess a b = Scoutess (Kleisli IO a b)
    deriving (Category, Arrow, ArrowApply, ArrowChoice, ArrowLoop, ArrowPlus, ArrowZero)

liftScoutess :: (a -> IO b) -> Scoutess a b
liftScoutess = Scoutess . Kleisli

data SourceSpec
    = SourceSpec { locations :: Set SourceLocation }
      deriving Show

data SourceLocation
    = Darcs -- ^ need to specify a repo
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
    { viPackageIdentifier :: PackageIdentifier
    , viVersionTag        :: Text
    , viSourceLocation    :: SourceLocation
    , viDependencies      :: [Dependency]
    }
    deriving (Show, Eq, Ord)

deriving instance Ord VersionRange
deriving instance Ord Dependency

data PriorRun = PriorRun
    {
    }
    deriving Show

data BuildSpec = BuildSpec
    {
    }
    deriving Show

data DependencyGraph = DependencyGraph
    { graph       :: Graph
    , association :: Bimap Vertex VersionInfo
    }

deriving instance Typeable2 Bimap
-- | Given that a 'Bimap' is just two 'Map's, this defintion is very similar to the one for 'Map'
instance (Data a, Data b, Ord a, Ord b) => Data (Bimap a b) where
    gfoldl     f z m = z B.fromList `f` B.toList m
    toConstr   _     = error "toConstr"
    gunfold    _ _   = error "gunfold"
    dataTypeOf _     = mkNoRepType "Data.Bimap.Bimap"
    dataCast2  f     = gcast2 f

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
fetchVersionsFrom Hackage = toVersionInfos <$> H.fetchAllVersions sourceConfig
    where toVersionInfos :: Set (Text,Text) -> Set VersionInfo
          -- ^ possibly defined in Scoutess.Service.Source.Hackage?
          toVersionInfos  = undefined
          sourceConfig    = undefined
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
--   currently takes the highest valid dependency
getImmDeps :: VersionSpec -> VersionInfo -> [VersionInfo]
getImmDeps (VersionSpec{versions}) (VersionInfo{viDependencies}) = map findDep viDependencies
    where findDep dep = S.findMax (S.filter (fitsDep dep) versions)
          -- ^ XXX: crashes with an unhelpful error if there are no valid dependencies
          fitsDep :: Dependency -> VersionInfo -> Bool
          fitsDep     (Dependency name range) vi = validName name vi && validVersion range vi
          validName    name                   vi = name == pkgName (viPackageIdentifier vi)
          validVersion range                  vi = withinRange (pkgVersion (viPackageIdentifier vi)) range


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
calculateChanges = undefined

updateLocalHackage :: Scoutess BuildSpec LocalHackageIndex
updateLocalHackage = undefined

build :: Scoutess (LocalHackageIndex, BuildSpec) BuildReport
build = undefined