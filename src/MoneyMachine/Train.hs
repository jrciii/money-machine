module MoneyMachine.Train where

import OANDA
import Candle

data Profitable = Bool
data Trade = Trade Candlestick Profitable

--simulateTrades :: [(Candlestick,[Candlestick])] -> [Trade]
