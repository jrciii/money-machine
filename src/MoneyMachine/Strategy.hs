{-# LANGUAGE TemplateHaskell #-}
module MoneyMachine.Strategy where

import           Control.Lens
import           Data.HashMap.Lazy
import           Data.Sequence
import           Data.Time.LocalTime
import           MoneyMachine.Candle
import           MoneyMachine.Order
import           MoneyMachine.Trade

data Strategy = Strategy
  { _strategyName :: String
  , _onTick       :: HashMap String ([Maybe Candle],Maybe Candle) -> Spread -> Commission -> [Order] -> [Trade] -> IO [Order]
  , _windowSize   :: Int
  }

makeLenses ''Strategy
