{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module MoneyMachine.Practice where
import           Control.Concurrent
import           Control.Exception.Lifted
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Control
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
import Control.Concurrent.Async
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


tryAnyIO :: IO a -> IO (Either SomeException a)
tryAnyIO action = withAsync action waitCatch

tryAny :: MonadBaseControl IO m => m a -> m (Either SomeException a)
tryAny action =
    liftBaseWith (\runInIO -> tryAnyIO (runInIO action)) >>=
    either (return . Left) (liftM Right . restoreM)

catchAny :: MonadBaseControl IO m => m a -> (SomeException -> m a) -> m a
catchAny action onE = tryAny action >>= either onE return

--go :: (Monad m) => OandaEnv -> AccountID -> CandlestickGranularity -> [String] -> Strategy -> Int -> Int -> THTC.TimeZone -> TI.TimeZone -> StateT (Maybe Candle) m ()
go env aid granularity instruments strat granSecs windowSecs windowSize tz tzti = (flip catchAny) (\e -> liftIO $ putStrLn $ "Got error: " ++ show e) $ do
    accountDetails <- makeOandaRequest $ oandaAccountDetails env aid
    let account = accountDetailsResponseAccount accountDetails
    let positions = accountOpenPositionCount account
    if positions > 0
    then do
      liftIO $ threadDelay $ 30 * 1000000
    else do
      to <-liftIO $ THC.getCurrentTime
      let toTime = THTC.addUTCTime (THC.fromSeconds (-3)) to
      let fromTime = THTC.addUTCTime windowSecs toTime
      let toZon = THTC.utcToZonedTime tz toTime
      let fromZon = THTC.utcToZonedTime tz fromTime
      let args = map (\i -> makeTestArgs (InstrumentName $ T.pack i) granularity fromZon toZon) instruments
      let reqs = map (makeOandaRequest . oandaCandles env) args
      resps <- sequence reqs
      lastTime <- get
      let thisTime = candlestickTime $ head $ candlestickResponseCandles $ head $ resps
      case lastTime of
        Just t | (t == thisTime) -> do
          liftIO $ threadDelay $ 30 * 1000000
        _ -> do
          put $ Just thisTime
          let candles = map ((map oandaCandleToMMCandle) . candlestickResponseCandles) resps
          let sorted = L.sortBy (\a b-> (_candleTime . head $ a) `compare` (_candleTime . head $ b )) candles
          let startSec = floor $ TIP.utcTimeToPOSIXSeconds $ TI.localTimeToUTC tzti $ _candleTime $ head $ head sorted
          let endSec = floor $ TIP.utcTimeToPOSIXSeconds $ TI.localTimeToUTC tzti $ _candleTime $ last $ head sorted
          let filled = map (fillGaps startSec endSec granSecs) candles
          let window = HM.fromList (zip instruments (map (\x -> (x,last x)) filled))
          --let fromLoc = (THTC.fromThyme $ THTC.utcToLocalTime tz fromTime)
          let toLoc = (THTC.fromThyme $ THTC.utcToLocalTime tz toTime)
          orders <- liftIO $ (_onTick strat) window (MO.Spread (MO.Points 0.00015)) (MO.Commission 0.0) [] []
          liftIO $ mapM_ (print . length) filled
          liftIO $ print orders
          let reqs = map (makeOandaRequest . (oandaCreateOrder env aid) . createOrderDetails) orders
          resps <- sequence reqs
          liftIO $ print resps
          liftIO $ threadDelay $ 30 * 1000000

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
