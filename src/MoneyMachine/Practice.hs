{-# LANGUAGE OverloadedStrings #-}

module MoneyMachine.Practice where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import qualified Data.HashMap.Lazy      as HM
import qualified Data.List              as L
import           Data.Maybe
import qualified Data.Text              as T
import qualified Data.Thyme             as TH
import qualified Data.Thyme.Clock       as THC
import qualified Data.Thyme.Clock.POSIX as THP
import qualified Data.Thyme.Time        as THT
import qualified Data.Thyme.Time.Core   as THTC
import qualified Data.Time              as TT
import qualified Data.Time.Clock.POSIX  as TIP
import qualified Data.Time.LocalTime    as TI
import           MoneyMachine.Candle
import qualified MoneyMachine.Order     as MO
import           MoneyMachine.Strategy
import           MoneyMachine.Trade
import           OANDA
import           Text.Printf
{-# ANN module ("HLint: ignore Redundant bracket"::String) #-}

practice :: OandaEnv -> AccountID -> CandlestickGranularity -> [String] -> Strategy -> IO ()
practice env aid granularity instruments strat =
  let windowSize = _windowSize strat
      granSecs = floor $ realToFrac $ granularityToDiffTime granularity
      windowSecs = THC.fromSeconds $ (-1) * windowSize * granSecs
      lastCandle = Nothing
      tz = TH.hoursToTimeZone (-4)
      tzti = TI.hoursToTimeZone (0)

  in forever $ do
      accountDetails <- makeOandaRequest $ oandaAccountDetails env aid
      let account = accountDetailsResponseAccount accountDetails
      let positions = accountOpenPositionCount account
      if positions > 0
            then do
                putStrLn "Open positions, sleeping"
                (threadDelay $ (windowSize * granSecs) * 1000000)
            else do
                to <- THC.getCurrentTime
                let toTime = THTC.addUTCTime (THC.fromSeconds (-3)) to
                let fromTime = THTC.addUTCTime windowSecs toTime
                let toZon = THTC.utcToZonedTime tz toTime
                let fromZon = THTC.utcToZonedTime tz fromTime
                let args = map (\i -> makeTestArgs (InstrumentName $ T.pack i) granularity fromZon toZon) instruments
                let reqs = map (makeOandaRequest . oandaCandles env) args
                resps <- sequence reqs
                let candles = map ((map oandaCandleToMMCandle) . candlestickResponseCandles) resps
                let sorted = L.sortBy (\a b-> (_candleTime . head $ a) `compare` (_candleTime . head $ b )) candles
                let startSec = floor $ TIP.utcTimeToPOSIXSeconds $ TI.localTimeToUTC tzti $ _candleTime $ head $ head sorted
                let endSec = floor $ TIP.utcTimeToPOSIXSeconds $ TI.localTimeToUTC tzti $ _candleTime $ last $ head sorted
                let filled = map (fillGaps startSec endSec granSecs) candles
                let window = HM.fromList (zip instruments filled)
                --let fromLoc = (THTC.fromThyme $ THTC.utcToLocalTime tz fromTime)
                let toLoc = (THTC.fromThyme $ THTC.utcToLocalTime tz toTime)
                orders <- (_onTick strat) toLoc window (MO.Spread (MO.Points 0.00015)) (MO.Commission 0.0) [] []
                mapM_ (print . length) filled
                putStrLn $ show orders
                let reqs = map (\o -> (makeOandaRequest $ (oandaCreateOrder env aid) $ createOrderDetails o)) orders
                resps <- sequence reqs
                putStrLn $ show resps
                threadDelay $ (windowSize * granSecs) * 1000000

{-placeOrders :: OandaEnv -> [Order] -> IO ()
placeOrders env [] = return ()
placeOrders env orders = -}


createOrderDetails :: MO.Order -> OrderRequest
createOrderDetails order =
    let sl = Just StopLossDetails
           { stopLossDetailsPrice = T.pack $ printf "%.5f" ((\(MO.Price p) -> p) (fromJust $ order^.MO.stopLoss))
           , stopLossDetailsTimeInForce = GTC
           , stopLossDetailsGtdTime = OandaZonedTime $ THT.utcToZonedTime (THTC.hoursToTimeZone 0) $ THT.posixSecondsToUTCTime $ fromIntegral 0
           , stopLossDetailsClientExtensions = Nothing
           }
        tp = Just TakeProfitDetails
           { takeProfitDetailsPrice = T.pack $ printf "%.5" ((\(MO.Price p) -> p) (fromJust $ order^.MO.takeProfit))
           , takeProfitDetailsTimeInForce = GTC
           , takeProfitDetailsGtdTime = OandaZonedTime $ THT.utcToZonedTime (THTC.hoursToTimeZone 0) $ THT.posixSecondsToUTCTime $ fromIntegral 0
           , takeProfitDetailsClientExtensions = Nothing
           }
        blankReq = orderRequest MARKET
    in blankReq
       & orderRequestInstrument .~ (Just $ InstrumentName $ T.pack $ MO._orderInstrument order)
       & orderRequestUnits .~ (Just $ fromIntegral $ (\(MO.Units u) -> u) (MO._orderUnits order))
       & orderRequestPrice .~ (Just $ PriceValue $ T.pack $ show $ (\(MO.Price p) -> p) (MO._orderPrice order))
       & orderRequestTakeProfitOnFill .~ tp
       & orderRequestStopLossOnFill .~ sl
