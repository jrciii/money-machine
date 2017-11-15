{-# LANGUAGE OverloadedStrings #-}

module MoneyMachine.Practice where

import           Control.Concurrent
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import qualified Data.HashMap.Lazy                as HM
import qualified Data.List                        as L
import           Data.Maybe
import qualified Data.Sequence                    as S
import qualified Data.Text                        as T
import qualified Data.Thyme                       as TH
import qualified Data.Thyme.Clock                 as THC
import qualified Data.Thyme.Clock.POSIX           as THP
import qualified Data.Thyme.Time                  as THT
import qualified Data.Thyme.Time.Core             as THTC
import qualified Data.Time                        as TT
import qualified Data.Time.Clock.POSIX            as TIP
import qualified Data.Time.LocalTime              as TI
import           MoneyMachine.Candle
import qualified MoneyMachine.Order               as MO
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
  in evalStateT (forever (go env aid granularity instruments strat granSecs windowSecs windowSize tz tzti)) lastCandle

--go :: (Monad m) => OandaEnv -> AccountID -> CandlestickGranularity -> [String] -> Strategy -> Int -> Int -> THTC.TimeZone -> TI.TimeZone -> StateT (Maybe Candle) m ()
go env aid granularity instruments strat granSecs windowSecs windowSize tz tzti = do
    accountDetails <- makeOandaRequest $ oandaAccountDetails env aid
    let account = accountDetailsResponseAccount accountDetails
    let positions = accountOpenPositionCount account
    if positions > 0
      then do
        liftIO $ putStrLn "Open positions, sleeping"
        liftIO $ threadDelay $ 30 * 1000000
      else do
        to <-liftIO $ THC.getCurrentTime
        let toTime = THTC.addUTCTime (THC.fromSeconds (-3)) to
        let fromTime = THTC.addUTCTime windowSecs toTime
        let toZon = THTC.utcToZonedTime tz toTime
        let fromZon = THTC.utcToZonedTime tz fromTime
        let args = map (\i -> makeTestArgs (InstrumentName $ T.pack i) granularity fromZon toZon) instruments
        let reqs = map (makeOandaRequest . oandaCandles env) args
        resps <- lift $ catchAny (sequence reqs) $ \e -> do
          putStrLn $ "Got an exception: " ++ show e
          return []
        case resps of
          [] ->  return ()
          _ -> do
            lastTime <- get
            let thisTime = candlestickTime $ head $ candlestickResponseCandles $ head $ resps
            case lastTime of
              Just t | (t == thisTime) -> do
                lift $ putStrLn "Same candle as last"
                lift $ threadDelay $ 30 * 1000000
              _ -> do
                       let candles = map ((map oandaCandleToMMCandle) . candlestickResponseCandles) resps
                       let sorted = L.sortBy (\a b-> (_candleTime . head $ a) `compare` (_candleTime . head $ b )) candles
                       let startSec = floor $ TIP.utcTimeToPOSIXSeconds $ TI.localTimeToUTC tzti $ _candleTime $ head $ head sorted
                       let endSec = floor $ TIP.utcTimeToPOSIXSeconds $ TI.localTimeToUTC tzti $ _candleTime $ last $ head sorted
                       let filled = map (fillGaps startSec endSec granSecs) candles
                       let window = HM.fromList (zip instruments (map (\x -> (x,last x)) filled))
                       --let fromLoc = (THTC.fromThyme $ THTC.utcToLocalTime tz fromTime)
                       let toLoc = (THTC.fromThyme $ THTC.utcToLocalTime tz toTime)
                       orders <- lift $ (_onTick strat) window (MO.Spread (MO.Points 0.00015)) (MO.Commission 0.0) [] []
                       lift $ mapM_ (print . length) filled
                       lift $ putStrLn $ show orders
                       let reqs = map (\o -> (makeOandaRequest $ (oandaCreateOrder env aid) $ createOrderDetails o)) orders
                       resps <- sequence reqs
                       lift $ putStrLn $ show resps
                       lift $ threadDelay $ 30 * 1000000

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

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
           { takeProfitDetailsPrice = T.pack $ printf "%.5f" ((\(MO.Price p) -> p) (fromJust $ order^.MO.takeProfit))
           , takeProfitDetailsTimeInForce = GTC
           , takeProfitDetailsGtdTime = OandaZonedTime $ THT.utcToZonedTime (THTC.hoursToTimeZone 0) $ THT.posixSecondsToUTCTime $ fromIntegral 0
           , takeProfitDetailsClientExtensions = Nothing
           }
        ts = Just TrailingStopLossDetails
           { trailingStopLossDetailsDistance = T.pack $ printf "%.5f" $ MO.unPoints $ (fromJust $ order^.MO.trailingStop)^.MO.trailAmount
           , trailingStopLossDetailsTimeInForce = GTC
           , trailingStopLossDetailsGtdTime = OandaZonedTime $ THT.utcToZonedTime (THTC.hoursToTimeZone 0) $ THT.posixSecondsToUTCTime $ fromIntegral 0
           , trailingStopLossDetailsClientExtensions = Nothing
           }
        blankReq = orderRequest MARKET
    in blankReq
       & orderRequestInstrument .~ (Just $ InstrumentName $ T.pack $ MO._orderInstrument order)
       & orderRequestUnits .~ (Just $ fromIntegral $ (\(MO.Units u) -> u) (MO._orderUnits order))
       & orderRequestPrice .~ (Just $ PriceValue $ T.pack $ show $ (\(MO.Price p) -> p) (MO._orderPrice order))
       & orderRequestTakeProfitOnFill .~ tp
       & orderRequestStopLossOnFill .~ sl
       & orderRequestTrailingStopLossOnFill .~ ts
