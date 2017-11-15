{-# LANGUAGE OverloadedStrings #-}
module MoneyMachine.HistData where

import qualified Data.ByteString.Char8      as C
import qualified Data.ByteString.Lex.Fractional as R
import           Data.Time
import           MoneyMachine.Candle

histDataToCandle :: (String -> LocalTime) -> C.ByteString -> Candle
histDataToCandle parser hist =
    let [d,o,h,l,c,_]  = C.split ',' hist
        Just (ro,_) = R.readDecimal o
        Just (rh,_) = R.readDecimal h
        Just (rl,_) = R.readDecimal l
        Just (rc,_) = R.readDecimal c
        rd = C.unpack d
    in Candle
       { _o = ro
       , _h = rh
       , _l = rl
       , _c = rc
       , _candleTime = parser rd
       }
