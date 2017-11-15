{-# LANGUAGE TemplateHaskell #-}
module MoneyMachine.Order where

import Control.Lens
import Data.Time.LocalTime

newtype Points = Points {unPoints :: Double} deriving (Show)
newtype Spread = Spread {unSpread :: Points} deriving (Show)
newtype Commission = Commission {unCommission :: Double} deriving (Show)
newtype Units = Units {unUnits :: Int} deriving (Show)
newtype Price = Price {unPrice :: Double} deriving (Show)
data TrailingStop = TrailingStop { _triggerPrice :: Price, _trailAmount :: Points } deriving (Show)
data OrderType = Long | Short deriving (Show)
data Order = Order {_orderType :: OrderType, _orderInstrument :: String, _orderPrice :: Price, _orderUnits :: Units, _stopLoss :: Maybe Price, _takeProfit :: Maybe Price, _trailingStop :: Maybe TrailingStop } deriving (Show)

makeLenses ''Order
makeLenses ''TrailingStop