-- | configuration for @Scoutess@
{-# LANGUAGE DeriveDataTypeable #-}
module Scoutess.Config where

import Data.Data (Data, Typeable)

-- | Scoutess Config
data ScoutessConfig = ScoutessConfig {
    }
    deriving (Show, Data, Typeable)
