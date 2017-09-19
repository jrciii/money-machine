{-# LANGUAGE TemplateHaskell #-}
module MoneyMachine.Order where

import Control.Lens
import Data.Time.LocalTime

data Points = Points Double deriving (Show)
data Spread = Spread Points deriving (Show)
data Commission = Commission Double deriving (Show)
data Units = Units Int deriving (Show)
data Price = Price Double deriving (Show)
data TrailingStop = TrailingStop { _triggerPrice :: Price, _trailAmount :: Points } deriving (Show)
data OrderType = Long | Short deriving (Show)
data Order = Order {_orderType :: OrderType, _orderInstrument :: String, _orderPrice :: Price, _orderUnits :: Units, _stopLoss :: Maybe Price, _takeProfit :: Maybe Price, _trailingStop :: Maybe TrailingStop } deriving (Show)

makeLenses ''Order
makeLenses ''TrailingStop