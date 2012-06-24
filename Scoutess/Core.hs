{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, DeriveDataTypeable #-}

module Scoutess.Core where

import Control.Arrow
import Control.Category (Category)
import Control.Monad.Error ()
-- ^ instance MonadPlus IO
import Data.Bimap (Bimap)
-- O(log n) bijection between 'Int's and 'VersionInfo's used in 'calculateDependencies'
import qualified Data.Bimap as B
import Data.Data (Typeable2, Data(..), mkNoRepType, gcast2)
import Data.Graph (Graph, Vertex)
import Data.Set (Set)
import Data.Text (Text)
import Distribution.Package
import Distribution.Version (VersionRange(..))

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
