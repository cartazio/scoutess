module Scoutess.Service.Source.Fetch where

import Control.Monad.Trans             (MonadIO(..))
import Scoutess.Service.Source.Core    (SourceConfig, SourceError, SourceInfo, SourceLocation(..))
import Scoutess.Service.Source.Darcs   (fetchDarcs)


-- | fetch the source
fetch :: (MonadIO m) =>
         SourceConfig
      -> SourceLocation -- ^ spec for where to find the source
      -> m (Either SourceError SourceInfo)
fetch sourceConfig sourceLocation =
    case sourceLocation of
      (Darcs location mTag) -> fetchDarcs sourceConfig location mTag
