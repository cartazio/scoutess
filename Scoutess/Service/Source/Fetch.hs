-- | high-level interface to source fetching
module Scoutess.Service.Source.Fetch where

import Control.Monad                   (liftM)
import Control.Monad.Trans             (MonadIO(..))
import Data.Either                     (partitionEithers)
import Scoutess.Core
import Scoutess.Service.Source.Darcs   (fetchDarcs)
import Scoutess.Service.Source.Hackage (fetchHackage)

-- | fetch the source
--
-- This call should be (mostly) idempotent. If you try to fetch/update
-- the same source twice, you should get back the same 'SourceInfo'.
--
-- For example, if you do a 'darcs pull' and get some changes you
-- should get some 'SourceInfo' back. If you then do another 'darcs
-- pull' and there are no changes, then you should get back the same
-- 'SourceInfo'.
--
-- If the call fails due to an Exception (such as the connecting being
-- broken, etc), then clearly that does not need to be idempotent.
--
-- Instead of returning '(Either SourceException SourceInfo)', we
-- could actually throw the exception. However, I think it makes more
-- sense to return the error, because if something does happen, we
-- want to include that in the 'BuildReport'.
--
-- Also, it makes it easier to attempt to fetch all the sources, even
-- if some fail.
fetchSrc :: (MonadIO m) =>
            SourceConfig
         -> VersionInfo -- ^ spec for where to find the source
         -> m (Either SourceException SourceInfo)
fetchSrc sourceConfig versionInfo =
    case viSourceLocation versionInfo of
      --(Darcs location mTag) -> fetchDarcs sourceConfig location mTag
      Hackage               -> fetchHackage sourceConfig versionInfo

-- | fetch multiple 'SourceLocation's
--
-- This function will attempt to fetch as many sources as it
-- can. Failures are reported in first element of the tuple, successes
-- in the second element.
fetchSrcs :: (MonadIO m) =>
             SourceConfig
          -> [VersionInfo] -- ^ spec for where to find the source
          -> m ([SourceException], [SourceInfo])
fetchSrcs sourceConfig versionInfos =
    liftM partitionEithers $ mapM (fetchSrc sourceConfig) versionInfos
