{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.KdTree.Static
import qualified Data.List                      as L
import qualified Data.Map                       as M
import qualified Data.Text                      as T
import qualified Data.Thyme.Time                as THT
import qualified Data.Thyme.Time.Core           as THC
import           Data.Time
import           Data.Time.Clock.POSIX
import           Debug.Trace
import           MoneyMachine.Backtest
import           MoneyMachine.Candle
import           MoneyMachine.Chart
import           MoneyMachine.Cluster
import           MoneyMachine.ClusterStrategy
import           MoneyMachine.Env
import           MoneyMachine.Order
import           MoneyMachine.Practice
import           MoneyMachine.Strategy
import           MoneyMachine.TechnicalAnalysis
import           OANDA
import           System.Environment
import           Text.Printf

main = do
  cargs <- getArgs
  let granularity = getGran $ cargs !! 0
  let instName = cargs !! 1
  let instName2 = cargs !! 2
  let ws = read $ cargs !! 3
  (env,aid) <- getPracticeEnv
  practice env aid granularity [instName,instName2] (clusterStrategy ws)

{-main = do
  cargs <- getArgs
  let granularity = getGran $ cargs !! 0
  let fileName = cargs !! 1
  let srlines = read $ cargs !! 2
  --let numcandles = read $ cargs !! 3
  let instName = InstrumentName $ T.pack $ cargs !! 4
  let instName2 = InstrumentName $ T.pack $ cargs !! 5
  let parser = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S"
  let tz = hoursToTimeZone 0
  let fromString = cargs !! 6
  let toString = cargs !! 7
  let fromTime = parser fromString :: UTCTime
  let toTime = parser toString :: UTCTime
  let startSec = floor $ utcTimeToPOSIXSeconds fromTime
  let endSec = floor $ utcTimeToPOSIXSeconds toTime
  let args = makeTestArgs instName granularity (THC.toThyme $ utcToZonedTime tz fromTime) (THC.toThyme $ utcToZonedTime tz toTime)
  let args2 = makeTestArgs instName2 granularity (THC.toThyme $ utcToZonedTime tz fromTime) (THC.toThyme $ utcToZonedTime tz toTime)
  --tickResps <- sequence $ getCandleResponses env instName granularity numcandles
  tickResp <- makeOandaRequest $ oandaCandles env args
  tickResp2 <- makeOandaRequest $ oandaCandles env args2
  let tickCandles = candlestickResponseCandles tickResp
  let tickCandles2 = candlestickResponseCandles tickResp2
  let mmCandles = map oandaCandleToMMCandle tickCandles
  let mmCandles2 = map oandaCandleToMMCandle tickCandles2
  --putStrLn $ show $ length mmCandles
  --putStrLn $ show $ length mmCandles2
  let instruments = L.sortBy (\a b -> (_candleTime . head . snd $ a) `compare` (_candleTime . head . snd $ b )) [(cargs !! 4,mmCandles),(cargs !! 5,mmCandles2)]
  let earliestTime = floor $ utcTimeToPOSIXSeconds $ localTimeToUTC (hoursToTimeZone 0) $ _candleTime . head . snd . head $ instruments
  let filled = map (\(k,v) -> (k,fillGaps startSec endSec (floor $ realToFrac $ granularityToDiffTime granularity) v)) instruments
  mapM_ (putStrLn . show . length . snd) filled
  let inits = init tickCandles
  let end = last tickCandles
  let last5init = reverse . (take 5) . reverse $ inits
  let kdTree = makeKdt inits
  let kn = kNearest kdTree 2 end
  putStrLn "=====5 before end"
  mapM_ (putStrLn . (++"\n") . show) last5init
  putStrLn "=====End"
  putStrLn $ show end
  putStrLn "=====Nearest S/R"
  putStrLn $ show $ kn !! 0
  putStrLn "=====Second nearest S/R"
  putStrLn $ show $ kn !! 1
  putStrLn "\n\n\n\n\n"
  let prices = candlesAsPrices tickCandles
  let sr = kmeansSRo tickCandles srlines
  putStrLn $ show $ length sr
  --let sr = [1.2,1.3]
  zt <- getZonedTime
  let time = zonedTimeToLocalTime zt
  profit <- backtest time filled clusterStrategy (Spread (Points 0.00015)) (Commission 0.0)
  --() <$ (makeSRChart fileName prices sr)
  printf "%.5f\n" profit
  --sequence . concat $ io
-}
getGran :: String -> CandlestickGranularity
getGran "S5"  = S5
getGran "S10" = S10
getGran "S15" = S15
getGran "S30" = S30
getGran "M1"  = M1
getGran "M2"  = M2
getGran "M4"  = M4
getGran "M5"  = M5
getGran "M10" = M10
getGran "M15" = M15
getGran "M30" = M30
getGran "H1"  = H1
getGran "H2"  = H2
getGran "H3"  = H3
getGran "H4"  = H4
getGran "H6"  = H6
getGran "H8"  = H8
getGran "H12" = H12
getGran "D"   = D
getGran "W"   = W
getGran "M"   = M

--main = do
--  let tickArgs = makeTickArgs "USD_JPY" S5 5000
--  tickResp <- makeOandaRequest $ oandaCandles env tickArgs
--  let tickCandles = candlestickResponseCandles tickResp
--  let (tFrom,tTo) = range tickCandles
--  let testArgs = makeTestArgs "USD_JPY" S30 (unOandaZonedTime tFrom) (unOandaZonedTime tTo)
--  testResp <- makeOandaRequest $ oandaCandles env testArgs
--  let testCandles = drop 1 $ candlestickResponseCandles testResp
--  let cleanedTicks = dropTooEarlyTicks (head testCandles) tickCandles
--  let intervals = makeIntervals cleanedTicks testCandles
--  putStrLn $ show $ (tFrom,tTo)
--  putStrLn $ show $ range $ tickCandles
--  putStrLn $ show $ range $ testCandles
--  putStrLn $ show $ last intervals

-- (\m -> map (flip M.lookup m) [1..8]) . foldr (\a -> M.insert a a) M.empty $ [2,4,5,8]
