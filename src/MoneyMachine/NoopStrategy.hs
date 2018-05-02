{-# LANGUAGE BangPatterns #-}
module MoneyMachine.NoopStrategy where

import qualified Data.HashMap.Strict     as HM
import           Data.Maybe
import           MoneyMachine.Candle
import           MoneyMachine.Debug
import           MoneyMachine.Order
import           MoneyMachine.Strategy
import           MoneyMachine.Trade
import Data.Sequence

noopStrategy ws = Strategy {_strategyName = "Cluster Strategy", _onTick = noopStratOnTick, _windowSize = ws}

noopStratOnTick :: HM.HashMap String ([Maybe Candle],Maybe Candle) -> Spread -> Commission -> [Order] -> [Trade] -> IO [Order]
noopStratOnTick market (Spread (Points spread)) commission os newTrades = return []
