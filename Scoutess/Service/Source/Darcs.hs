{-# LANGUAGE OverloadedStrings #-}
-- | fetch (cabal) source using @darcs@
--
-- <http://darcs.net>
module Scoutess.Service.Source.Darcs where

import Control.Monad.Trans          (MonadIO(..))
import Data.Text                    (Text)
import qualified Data.Text          as Text
import Scoutess.Service.Source.Core (SourceConfig, SourceException, SourceInfo)

fetchDarcs :: (MonadIO m) =>
              SourceConfig -- ^ 'SourceConfig'
           -> Text         -- ^ location of darcs repo (e.g. @http:\/\/example.org\/repo@, @ssh:\/\/user\@example.org/srv/darcs/repo@)
           -> Maybe Text   -- ^ optional darcs tag
           -> m (Either SourceException SourceInfo)
fetchDarcs sourceConfig location tag =
    undefined
