module MoneyMachine.HistData where

import           Data.List.Split
import qualified Data.Text           as T
import qualified Data.Text.Read      as TR
import           Data.Time
import           MoneyMachine.Candle

histDataToCandle :: (String -> LocalTime) -> T.Text -> Candle
histDataToCandle parser hist =
    let [d,o,h,l,c,_]  = T.split (==',') hist
        Right (ro,_) = TR.double o
        Right (rh,_) = TR.double h
        Right (rl,_) = TR.double l
        Right (rc,_) = TR.double c
    in Candle
       { _o = ro
       , _h = rh
       , _l = rl
       , _c = rc
       , _candleTime = parser $ T.unpack d
       }
