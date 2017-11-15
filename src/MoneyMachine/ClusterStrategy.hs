{-# LANGUAGE BangPatterns #-}
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
import qualified Data.Sequence         as S
import           Data.TALib
import qualified Data.Thyme.LocalTime  as TH
import qualified Data.Thyme.Time.Core  as TC
import qualified Data.Time.LocalTime   as TI
import qualified Data.Vector           as V
import qualified Data.Vector.Storable  as VS
import           Foreign.C.Types
import           Text.Printf
import qualified Data.Foldable as F

clusterStrategy ws = Strategy {_strategyName = "Cluster Strategy", _onTick = clusterStratOnTick, _windowSize = ws}

clusterStratOnTick :: HM.HashMap String ([Maybe Candle],Maybe Candle) -> Spread -> Commission -> [Order] -> [Trade] -> IO [Order]
clusterStratOnTick market (Spread (Points spread)) commission os newTrades = do
  let eurUsdCandles = foldr (\x a -> case x of Nothing -> a; Just v -> v:a) [] $ fst $ fromJust $ HM.lookup "EUR_USD" market
  let gbpUsdCandles = foldr (\x a -> case x of Nothing -> a; Just v -> v:a) [] $ fst $ fromJust $ HM.lookup "GBP_USD" market
  case (eurUsdCandles,gbpUsdCandles) of
    ([],_) -> return []
    (_,[]) -> return []
    _ -> do
      let ecPrices = map (coerce . _c) eurUsdCandles
      let gcPrices = map (coerce . _c) gbpUsdCandles
      let ehPrices = map (coerce . _h) eurUsdCandles
      let ghPrices = map (coerce . _h) gbpUsdCandles
      let elPrices = map (coerce . _l) eurUsdCandles
      let glPrices = map (coerce . _l) gbpUsdCandles
      let ecv = VS.fromList ecPrices :: VS.Vector CDouble
      let gcv = VS.fromList gcPrices :: VS.Vector CDouble
      let ehv = VS.fromList ehPrices :: VS.Vector CDouble
      let ghv = VS.fromList ghPrices :: VS.Vector CDouble
      let elv = VS.fromList elPrices :: VS.Vector CDouble
      let glv = VS.fromList glPrices :: VS.Vector CDouble
      let ePrice = coerce $ VS.last ecv
      let gPrice = coerce $ VS.last gcv
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
      let atrS = 140
      let fmaS = 80
      let mmaS = 130
      let smaS = 210
      let slice' v s = VS.slice (VS.length v - s - 1) (s + 1) v
      let slice'' v s = VS.slice (VS.length v - s - 1) (s) v
      Right (_,_,eatrs) <- ta_atr (slice' ehv atrS) (slice' elv atrS) (slice' ecv atrS) atrS
      Right (_,_,gatrs) <- ta_atr (slice' ghv atrS) (slice' glv atrS) (slice' gcv atrS) atrS
      Right (_,_,fesmas) <- ta_ema (slice'' ecv fmaS) fmaS
      Right (_,_,fgsmas) <- ta_ema (slice'' gcv fmaS) fmaS
      Right (_,_,mesmas) <- ta_ema (slice'' ecv mmaS) mmaS
      Right (_,_,mgsmas) <- ta_ema (slice'' gcv mmaS) mmaS
      Right (_,_,sesmas) <- ta_ema (slice'' ecv smaS) smaS
      Right (_,_,sgsmas) <- ta_ema (slice'' gcv smaS) smaS
      let eatr = coerce $ VS.head eatrs :: Double
      let gatr = coerce $ VS.head gatrs :: Double
      let fesma = coerce $ VS.head fesmas :: Double
      let fgsma = coerce $ VS.head fgsmas :: Double
      let mesma = coerce $ VS.head mesmas :: Double
      let mgsma = coerce $ VS.head mgsmas :: Double
      let sesma = coerce $ VS.head sesmas :: Double
      let sgsma = coerce $ VS.head sgsmas :: Double
      --let eatr = abs ((maximum ePrices) - (minimum ePrices))
      --let gatr = avg gPrices
      --let eavg = avg ePrices
      --let gavg = avg gPrices
      let esl = eatr
      let gsl = gatr
      let slf = 2
      let tpf = 6
      let result | (esl > spread) && (fesma < mesma && mesma < sesma) && (ePrice > (head eClust)) && (gPrice < (last gClust)) && (eSRNumL >= gSRNumU) = [makeOrder Long "EUR_USD" ePrice 1000 (ePrice - esl * slf) (ePrice + esl*tpf)]
                 | (gsl > spread) && (fgsma < mgsma && mgsma < sgsma) && (gPrice > (head gClust)) && (ePrice < (last eClust)) && (gSRNumL >= eSRNumU) = [makeOrder Long "GBP_USD" gPrice 1000 (gPrice - gsl * slf) (gPrice + gsl*tpf)]
                 | (esl > spread) && (fesma > mesma && mesma > sesma) && (ePrice < (last eClust)) && (gPrice > (head gClust)) && (eSRNumU <= gSRNumL) = [makeOrder Short "EUR_USD" ePrice (-1000) (ePrice + esl * slf) (ePrice - esl*tpf)]
                 | (gsl > spread) && (fgsma > mgsma && mgsma > sgsma) && (gPrice < (last gClust)) && (ePrice > (head eClust)) && (gSRNumU <= eSRNumL) = [makeOrder Short "GBP_USD" gPrice (-1000) (gPrice + gsl * slf) (gPrice + gsl*tpf)]
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
