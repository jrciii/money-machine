{-# LANGUAGE TemplateHaskell #-}
module MoneyMachine.Strategy where

import Control.Lens
import MoneyMachine.Order
import MoneyMachine.Trade
import MoneyMachine.Candle
import Data.HashMap.Lazy
import Data.Time.LocalTime
import Data.Vector

data Strategy = Strategy
  { _strategyName :: String
  , _onTick       :: HashMap String (Vector (Maybe Candle)) -> Spread -> Commission -> [Order] -> [Trade] -> IO [Order]
  , _windowSize   :: Int
  }

makeLenses ''Strategy