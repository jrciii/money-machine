module MoneyMachine.ClusterStrategy where

import qualified Data.HashMap.Lazy     as HM
import           Data.List
import           Data.Maybe
import           Data.Time.Format
import           Data.Time.LocalTime
import           Debug.Trace
import           MoneyMachine.Candle
import           MoneyMachine.Chart
import           MoneyMachine.Cluster
import           MoneyMachine.Debug
import           MoneyMachine.Order
import           MoneyMachine.Strategy
import           MoneyMachine.Trade
import           System.Directory      as D

import qualified Data.Thyme.LocalTime  as TH
import qualified Data.Thyme.Time.Core  as TC
import qualified Data.Time.LocalTime   as TI
import           Text.Printf
--import qualified Data.TALib            as TA

clusterStrategy ws = Strategy {_strategyName = "Cluster Strategy", _onTick = clusterStratOnTick, _windowSize = ws}

clusterStratOnTick :: LocalTime -> HM.HashMap String [Maybe Candle] -> Spread -> Commission -> [Order] -> [Trade] -> IO [Order]
clusterStratOnTick starttime market spread commission os newTrades = do
  let eurUsdCandles = map fromJust $ filter (\a -> case a of Nothing -> False; _ -> True) $ fromJust $ HM.lookup "EUR_USD" market
  let gbpUsdCandles = map fromJust $ filter (\a -> case a of Nothing -> False; _ -> True) $ fromJust $ HM.lookup "GBP_USD" market
  case (eurUsdCandles,gbpUsdCandles) of
    ([],_) -> return []
    (_,[]) -> return []
    _ -> do
      let feCandle = head eurUsdCandles
      let fgCandle = head gbpUsdCandles
      let eCandle = last eurUsdCandles
      let gCandle = last gbpUsdCandles
      let ePrice = _c eCandle
      let gPrice = _c gCandle
      let ePrices = map _c eurUsdCandles
      let gPrices = map _c gbpUsdCandles
      let eClusters = sort $ kmeansSR eurUsdCandles _c 2
      let gClusters = sort $ kmeansSR gbpUsdCandles _c 2
      let ecl = head eClusters
      let ecg = last eClusters
      let gcl = head gClusters
      let gcg = last gClusters
      let eClust = fst $ findNearest ePrice eClusters
      let gClust = fst $ findNearest gPrice gClusters
      let eSRNumU = (fromJust $ elemIndex (head eClust) eClusters) :: Int
      let eSRNumL = (fromJust $ elemIndex (last eClust) eClusters) :: Int
      let gSRNumU = (fromJust $ elemIndex (head gClust) gClusters) :: Int
      let gSRNumL = (fromJust $ elemIndex (last gClust) gClusters) :: Int
      --let eatr = abs ((maximum ePrices) - (minimum ePrices))
      --let gatr = avg gPrices
      let eavg = avg ePrices
      let gavg = avg gPrices
      let result | (ePrice > (head eClust)) && (gPrice < (head gClust)) && (gSRNumU <= eSRNumL) && (ePrice > eavg) = [makeOrder Long "EUR_USD" ePrice 1000 (ePrice - 0.0008) (ePrice + 0.0016)]
                 | (gPrice > (head gClust)) && (ePrice < (head eClust)) && (eSRNumU <= gSRNumL) && (gPrice > gavg)= [makeOrder Long "GBP_USD" gPrice 1000 (gPrice - 0.0008) (gPrice + 0.0016)]
                 | (ePrice > (last eClust)) && (gPrice < (last gClust)) && (gSRNumL >= eSRNumU) && (ePrice < eavg) = [makeOrder Short "EUR_USD" ePrice (-1000) (ePrice + 0.0008) (ePrice - 0.0016)]
                 | (gPrice > (last gClust)) && (ePrice < (last eClust)) && (eSRNumU >= gSRNumL) && (gPrice < gavg)= [makeOrder Short "GBP_USD" gPrice (-1000) (gPrice + 0.0008) (gPrice + 0.0016)]
                 |  otherwise = []
      --let result | (ePrice > ecl) && (gPrice < gcl) = [makeOrder Long "EUR_USD" ePrice 1000 (ePrice - 0.0055) (ePrice + 0.009)]
      --           | (gPrice > gcl) && (ePrice < ecl) = [makeOrder Long "GBP_USD" gPrice 1000 (gPrice - 0.0055) (gPrice + 0.009)]
      --           | (ePrice < ecg) && (gPrice > gcg) = [makeOrder Short "EUR_USD" ePrice (-1000) (ePrice + 0.0055) (ePrice - 0.009)]
      --           | (gPrice < gcg) && (ePrice > ecg) = [makeOrder Short "GBP_USD" gPrice (-1000) (gPrice + 0.0055) (gPrice + 0.009)]
      --           | otherwise = []

      --let result = [makeOrder Long "EUR_USD" ePrice 1000 (ePrice - 0.0002) (ePrice + 0.0004)]
      --makeChart starttime "EUR_USD" eurUsdCandles eClusters
      --makeChart starttime "GBP_USD" gbpUsdCandles gClusters
      case os of
        [] -> return result
        _  -> return []

findNearest :: (Num t, Ord t) => t -> [t] -> ([t], t)
findNearest _ [] = ([],0)
findNearest n (x:xs) = foldl (\(v,d) a -> checkNearer v d n a) ([n],abs(n - x)) (xs)
checkNearer v d n x =
  let diff = abs(n - x)
  in if diff < d then ([x],diff) else if diff == d then (x : v,diff) else (v,d)

makeOrder otype inst price u sl tp = trace ("orderin") $ traceThis(Order {_orderType = otype, _orderInstrument = inst, _orderPrice = Price price, _orderUnits=Units u, _stopLoss= Just $ Price sl, _takeProfit= Just $ Price tp, _trailingStop=Nothing})

{-makeChart :: LocalTime -> String -> [Candle] -> [Double] -> IO ()
makeChart starttime instName candles sr =
  let timePrice = map (\c -> (_candleTime c,_c c)) candles
      folder = formatTime defaultTimeLocale "%F_%H-%M-%S" starttime
      time = formatTime defaultTimeLocale "%F_%H-%M-%S" $ _candleTime . head $ candles
      fileName = "clustercharts/" ++ folder ++ "/" ++ instName ++ "/" ++ time ++ ".png"
  in do
    D.createDirectoryIfMissing True $ "clustercharts/" ++ folder ++ "/" ++ instName
    makeSRWindowChart fileName timePrice sr
-}
avg l =
    let (sum,count) = foldl' (\(s,c) x -> (s+x,c+1)) (0,0) l
    in (sum / count)
