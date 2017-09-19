{-# LANGUAGE TemplateHaskell #-}
module MoneyMachine.Trade where

import Control.Lens
import MoneyMachine.Order

data Trade = Trade {_tradeInstrument :: String, _tradeOpen :: Price, _tradeClose :: Price, _tradeUnits :: Units, _tradeProfit :: Points} deriving (Show)

makeLenses ''Trade