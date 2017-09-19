{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module MoneyMachine.Candle where

import OANDA
import qualified Data.Thyme.LocalTime as TH
import qualified Data.Time.LocalTime as TI
import Data.Maybe
import Control.Monad.IO.Class
import Control.Lens
import qualified Data.Text as T
import qualified Data.Thyme.Time.Core as TC
import Data.Time.Clock.POSIX
import qualified Data.Map as M

data Candle = Candle
  { _o    :: Double
  , _h    :: Double
  , _l    :: Double
  , _c    :: Double
  , _candleTime :: TI.LocalTime
  } deriving (Show)

makeLenses ''Candle
getCandleResponses :: (MonadIO m) => OandaEnv -> InstrumentName -> CandlestickGranularity -> Int -> m CandlestickResponse
getCandleResponses env iName gran count = makeOandaRequest $ oandaCandles env $ makeTickArgs iName gran count

makeTickArgs :: InstrumentName -> CandlestickGranularity -> Int -> CandlestickArgs
makeTickArgs iName gran count = CandlestickArgs {
    _candlestickArgsInstrument = iName,
    _candlestickArgsPrice = Just "B",
    _candlestickArgsGranularity = gran,
    _candlestickArgsCount = Just count,
    _candlestickArgsFrom = Nothing,
    _candlestickArgsTo = Nothing,
    _candlestickArgsSmooth = Just False,
    _candlestickArgsIncludeFirst = Just True,
    _candlestickArgsDailyAlignment = Nothing,
    _candlestickArgsAlignmentTimezone = Nothing,
    _candlestickArgsWeeklyAlignment = Nothing
  }

makeTestArgs :: InstrumentName -> CandlestickGranularity -> TH.ZonedTime -> TH.ZonedTime -> CandlestickArgs
makeTestArgs iName cGran from to = CandlestickArgs {
      _candlestickArgsInstrument = iName,
      _candlestickArgsPrice = Just "B",
      _candlestickArgsGranularity = cGran,
      _candlestickArgsCount = Nothing,
      _candlestickArgsFrom = Just from,
      _candlestickArgsTo = Just to,
      _candlestickArgsSmooth = Just False,
      _candlestickArgsIncludeFirst = Just True,
      _candlestickArgsDailyAlignment = Nothing,
      _candlestickArgsAlignmentTimezone = Nothing,
      _candlestickArgsWeeklyAlignment = Nothing
    }

range :: [Candlestick] -> (OandaZonedTime, OandaZonedTime)
range cs = (candlestickTime . head $ cs, candlestickTime . last $ cs)

getCandleTime :: Candlestick -> TH.ZonedTime
getCandleTime = unOandaZonedTime . candlestickTime

dropTooEarlyTicks :: Candlestick -> [Candlestick] -> [Candlestick]
dropTooEarlyTicks firstTest ticks =
  let testTime = getCandleTime firstTest
  in dropWhile ((<testTime) . getCandleTime) ticks

-- assumes both lists are sorted and start with same number
makeIntervals :: [Candlestick] -> [Candlestick] -> [(Candlestick,[Candlestick])]
makeIntervals _ [] = []
makeIntervals xs (y:[]) = [(y,xs)]
makeIntervals xs (y1:y2:ys) =
  let (l,r) = irange xs y1 y2
  in l : makeIntervals r (y2:ys)

irange :: [Candlestick] -> Candlestick -> Candlestick -> ((Candlestick,[Candlestick]),[Candlestick])
irange [] lo hi = ((lo,[]),[])
irange xs lo hi =
  let hiTime = getCandleTime hi
      (interval,rest) = span ((<hiTime) . getCandleTime) xs
  in ((lo,interval),rest)

candlesAsPrices :: [Candlestick] -> [(TH.LocalTime, Double)]
candlesAsPrices cs = map candleToPrice cs

candleToPrice :: Candlestick -> (TH.LocalTime, Double)
candleToPrice c = (TH.zonedTimeToLocalTime $ unOandaZonedTime $ candlestickTime c, read $ T.unpack $ unPriceValue $ candlestickDataC $ fromJust $ candlestickBid c)

oandaCandleToMMCandle :: Candlestick -> Candle
oandaCandleToMMCandle o =
  let bid = (fromJust . candlestickBid) o
  in Candle
    { _o = read . T.unpack . unPriceValue . candlestickDataO $ bid
    , _h = read . T.unpack . unPriceValue . candlestickDataH $ bid
    , _l = read . T.unpack . unPriceValue . candlestickDataL $ bid
    , _c = read . T.unpack . unPriceValue . candlestickDataC $ bid
    , _candleTime = TI.zonedTimeToLocalTime . TC.fromThyme . unOandaZonedTime . candlestickTime $ o}

fillGaps :: Int -> Int -> Int -> [Candle] -> [Maybe Candle]
fillGaps _ _ _ [] = []
fillGaps start end step candles =
  let steps = [start,start+step..end]
  in (\m -> map (flip M.lookup m) steps) . (foldr(\a -> M.insert (getUtcTimeSeconds a) a) M.empty) $ candles
  where getUtcTimeSeconds candle = floor $ utcTimeToPOSIXSeconds $ TI.localTimeToUTC (TI.hoursToTimeZone 0) (_candleTime candle)
