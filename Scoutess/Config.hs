{-# LANGUAGE DeriveDataTypeable #-}
module Scoutess.Config where

import Data.Data (Data, Typeable)

data ScoutessConfig = ScoutessConfig {
    }
    deriving (Show, Data, Typeable)
