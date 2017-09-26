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

import           Data.Coerce
import           Data.TALib
import qualified Data.Thyme.LocalTime  as TH
import qualified Data.Thyme.Time.Core  as TC
import qualified Data.Time.LocalTime   as TI
import qualified Data.Vector.Storable  as V
import           Foreign.C.Types
import           Text.Printf

clusterStrategy ws = Strategy {_strategyName = "Cluster Strategy", _onTick = clusterStratOnTick, _windowSize = ws}

clusterStratOnTick :: HM.HashMap String [Maybe Candle] -> Spread -> Commission -> [Order] -> [Trade] -> IO [Order]
clusterStratOnTick market (Spread (Points spread)) commission os newTrades = do
  let ws = length . (HM.lookup "EUR_USD") $ market
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
      let eClusters = sort $ kmeansSR eurUsdCandles _c 3
      let gClusters = sort $ kmeansSR gbpUsdCandles _c 3
      let ecl = head eClusters
      let ecg = last eClusters
      let gcl = head gClusters
      let gcg = last gClusters
      let eClust = fst $ findNearest ePrice eClusters
      let gClust = fst $ findNearest gPrice gClusters
      let eSRNumU = (fromJust $ elemIndex (last eClust) eClusters) :: Int
      let eSRNumL = (fromJust $ elemIndex (head eClust) eClusters) :: Int
      let gSRNumU = (fromJust $ elemIndex (last gClust) gClusters) :: Int
      let gSRNumL = (fromJust $ elemIndex (head gClust) gClusters) :: Int
      let elength = length eurUsdCandles
      let glength = length gbpUsdCandles
      let ehs = map _h eurUsdCandles
      let els = map _l eurUsdCandles
      let ghs = map _h gbpUsdCandles
      let gls = map _l gbpUsdCandles
      let ecv = V.fromListN elength (map coerce (ePrices))
      let gcv = V.fromListN glength (map coerce (gPrices))
      Right (_,_,eatrs) <- ta_atr (V.fromListN elength (map coerce (ehs))) (V.fromListN elength (map coerce (els))) (V.fromListN elength (map coerce (ePrices))) 13
      Right (_,_,gatrs) <- ta_atr (V.fromListN glength (map coerce ghs)) (V.fromListN glength (map coerce gls)) (V.fromListN glength (map coerce gPrices)) 13
      Right (_,_,fesmas) <- ta_sma ecv 8
      Right (_,_,fgsmas) <- ta_sma gcv 8
      Right (_,_,mesmas) <- ta_sma ecv 13
      Right (_,_,mgsmas) <- ta_sma gcv 13
      Right (_,_,sesmas) <- ta_sma ecv 21
      Right (_,_,sgsmas) <- ta_sma gcv 21
      let eatr = coerce $ V.head (eatrs) :: Double
      let gatr = coerce $ V.head (gatrs) :: Double
      let fesma = coerce $ V.head fesmas :: Double
      let fgsma = coerce $ V.head fgsmas :: Double
      let mesma = coerce $ V.head mesmas :: Double
      let mgsma = coerce $ V.head mgsmas :: Double
      let sesma = coerce $ V.head sesmas :: Double
      let sgsma = coerce $ V.head sgsmas :: Double
      --let eatr = abs ((maximum ePrices) - (minimum ePrices))
      --let gatr = avg gPrices
      let eavg = avg ePrices
      let gavg = avg gPrices
      let esl = eatr * 2
      let gsl = gatr * 2
      let result | (esl > spread) && (fesma < mesma && mesma < sesma) && (ePrice > (head eClust)) && (gPrice < (last gClust)) && (eSRNumL >= gSRNumU) = [makeOrder Long "EUR_USD" ePrice 1000 (ePrice - esl) (ePrice + esl*3)]
                 | (gsl > spread) && (fgsma < mgsma && mgsma < sgsma) && (gPrice > (head gClust)) && (ePrice < (last eClust)) && (gSRNumL >= eSRNumU) = [makeOrder Long "GBP_USD" gPrice 1000 (gPrice - gsl) (gPrice + gsl*3)]
                 | (esl > spread) && (fesma > mesma && mesma > sesma) && (ePrice < (last eClust)) && (gPrice > (head gClust)) && (eSRNumU <= gSRNumL) = [makeOrder Short "EUR_USD" ePrice (-1000) (ePrice + esl) (ePrice - esl*3)]
                 | (gsl > spread) && (fgsma > mgsma && mgsma > sgsma) && (gPrice < (last gClust)) && (ePrice > (head eClust)) && (gSRNumU <= eSRNumL) = [makeOrder Short "GBP_USD" gPrice (-1000) (gPrice + gsl) (gPrice + gsl*3)]
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
  in if diff < d then ([x],diff) else if diff == d then (v ++ [x],diff) else (v,d)

makeOrder otype inst price u sl tp = Order {_orderType = otype, _orderInstrument = inst, _orderPrice = Price price, _orderUnits=Units u, _stopLoss= Just $ Price sl, _takeProfit= Just $ Price tp, _trailingStop=Nothing}

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
