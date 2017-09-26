module MoneyMachine.HistData where

import           Data.List.Split
import           Data.Time
import           MoneyMachine.Candle

histDataToCandle :: String -> Candle
histDataToCandle hist =
    let s = splitOn "," hist
        d = s !! 0
        o = s !! 1
        h = s !! 2
        l = s !! 3
        c = s !! 4
        parser = parseTimeOrError True defaultTimeLocale "%d.%m.%Y %H:%M:%S.000"
        tz = hoursToTimeZone (0)
    in Candle
       { _o = read o
       , _h = read h
       , _l = read l
       , _c = read c
       , _candleTime = utcToLocalTime tz $ parser d
       }
