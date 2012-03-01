module Scoutess.Service.Source.Fetch where

import Control.Monad.Trans             (MonadIO(..))
import Scoutess.Service.Source.Core    (SourceConfig, SourceException, SourceInfo, SourceLocation(..))
import Scoutess.Service.Source.Darcs   (fetchDarcs)


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
fetch :: (MonadIO m) =>
         SourceConfig
      -> SourceLocation -- ^ spec for where to find the source
      -> m (Either SourceException SourceInfo)
fetch sourceConfig sourceLocation =
    case sourceLocation of
      (Darcs location mTag) -> fetchDarcs sourceConfig location mTag
