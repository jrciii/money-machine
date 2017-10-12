module MoneyMachine.HistData where

import           Data.List.Split
import           Data.Time
import           MoneyMachine.Candle

histDataToCandle :: (String -> LocalTime) -> String -> Candle
histDataToCandle parser hist =
    let [d,o,h,l,c,_]  = splitOn "," hist
    in Candle
       { _o = read o
       , _h = read h
       , _l = read l
       , _c = read c
       , _candleTime = parser d
       }
