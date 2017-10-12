{-# LANGUAGE TemplateHaskell #-}
module MoneyMachine.Strategy where

import Control.Lens
import MoneyMachine.Order
import MoneyMachine.Trade
import MoneyMachine.Candle
import Data.HashMap.Lazy
import Data.Time.LocalTime

data Strategy = Strategy
  { _strategyName :: String
  , _onTick       :: LocalTime -> HashMap String [Maybe Candle] -> Spread -> Commission -> [Order] -> [Trade] -> IO [Order]
  , _windowSize   :: Int
  }

makeLenses ''Strategy