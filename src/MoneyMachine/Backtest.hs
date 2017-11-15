module MoneyMachine.Backtest where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.RWS.Strict
import qualified Data.ByteString.Char8          as C
import           Data.Either
import qualified Data.HashMap.Strict            as HM
import qualified Data.List                      as L
import           Data.Maybe
import qualified Data.Sequence                  as S
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as TIO
import           Data.Time
import           Data.Time.Calendar.OrdinalDate
import           MoneyMachine.Candle
import           MoneyMachine.HistData
import           MoneyMachine.Order
import           MoneyMachine.Strategy
import           MoneyMachine.Trade
data Env = Env
           { _spread     :: !Spread
           , _commission :: !Commission
           }

backtest :: Strategy -> [String] -> [String] -> Int -> IO Double
backtest strat files instLabels ws = do
    contents <- mapM C.readFile files
    let parser = parseTimeOrError True defaultTimeLocale "%d.%m.%Y %H:%M:%S.000" :: String -> LocalTime
    let windowed = map (window ws . map Just . filter (liftA2 (&&) (>0) (<6) . snd . sundayStartWeek . localDay . _candleTime)
                     . map (histDataToCandle parser) . drop 1 . C.lines) contents
    let zipped = map (\(a,b) -> map (\x -> (a,x)) b) $ zip instLabels windowed
    let transposed = L.transpose zipped
    let env = Env {_spread = Spread (Points 0.00015), _commission = Commission 0.0}
    ((openOrders,trades),log) <- execRWST (runBacktest strat transposed) env ([],[])
    return $ L.foldl' (\profit trade -> profit + (\(Points p) -> p) (_tradeProfit trade) - (\( Spread (Points s)) -> s) (_spread env)) 0 trades

runBacktest :: Strategy -> [[(String,([Maybe Candle],Maybe Candle))]] -> RWST Env (S.Seq String) ([Order],[Trade]) IO ()
runBacktest _ [] = return ()
runBacktest strat (w:ws) =
    do Env spread commission <- ask
       (openOrders,existingTrades) <- get
       let (remainingOrders,newTrades) = checkOpenOrders openOrders w
       let market = HM.fromList w
       let trades = newTrades ++ existingTrades
       newOrders <- liftIO $ _onTick strat market spread commission remainingOrders trades
       let orders = newOrders ++ remainingOrders
       put (orders,trades)
       runBacktest strat ws

checkOpenOrders :: [Order] -> [(String,([Maybe Candle],Maybe Candle))] -> ([Order],[Trade])
checkOpenOrders [] _ = ([],[])
checkOpenOrders orders window =
  let checked = map (checkOrder window) orders
  in partitionEithers checked

checkOrder :: [(String,([Maybe Candle],Maybe Candle))] -> Order -> Either Order Trade
checkOrder window order =
    let (_,candle) = fromJust . L.lookup (_orderInstrument order) $ window
        Units units = _orderUnits order
        l = _l $ fromJust candle
        h = _h $ fromJust candle
        c = _c $ fromJust candle
        Price tp = fromJust . _takeProfit $ order
        Price sl = fromJust . _stopLoss $ order
        Price bp = _orderPrice order
        checkLong | (l <= sl) = Just $ closeTrade (_orderUnits order) (_orderInstrument order) (_orderPrice order) (Price sl) (sl - bp)
                  | (h >= tp) = Just $ closeTrade (_orderUnits order) (_orderInstrument order) (_orderPrice order) (Price tp) (tp - bp)
                  | otherwise = Nothing
        checkShort | (h >= sl) = Just $ closeTrade (_orderUnits order) (_orderInstrument order) (_orderPrice order) (Price sl) (bp - sl)
                   | (l <= tp) = Just $ closeTrade (_orderUnits order) (_orderInstrument order) (_orderPrice order) (Price tp) (bp - tp)
                   | otherwise = Nothing
        result =  if units > 0 then checkLong else checkShort
    in case candle of
        Nothing -> Left order
        _       -> maybe (Left (updateTrailingStopLoss (Price c) order)) Right result

updateTrailingStopLoss :: Price -> Order -> Order
updateTrailingStopLoss _ o@Order{_trailingStop = Nothing} = o
updateTrailingStopLoss (Price p) o@Order{_orderUnits = Units u,_stopLoss=sl,_trailingStop=Just TrailingStop {_triggerPrice = Price(tp), _trailAmount = Points ta}}
  | u > 0 && p >= tp =
    case sl of
      Nothing -> o & stopLoss .~ Just (Price (p - ta))
      Just (Price slp)
        | (p - ta) > slp -> o & stopLoss .~ Just (Price (p - ta))
        | otherwise -> o
  | u < 0 && p <= tp =
    case sl of
        Nothing -> o & stopLoss .~ Just (Price (p + ta))
        Just (Price slp)
          | (p + ta) < slp -> o & stopLoss .~ Just (Price (p + ta))
          | otherwise -> o
  | otherwise = o


closeTrade :: Units -> String -> Price -> Price -> Double -> Trade
closeTrade units inst open close profit = Trade {_tradeInstrument = inst, _tradeOpen = open, _tradeClose = close, _tradeUnits = units, _tradeProfit = Points profit}

lazyFmapTuple f ~(x, y) = (x, f y)

window :: Int -> [a] -> [([a],a)]
window n xs =
  let (l,b) = takeN n [] undefined xs
  in
    case b of Nothing -> []
              Just v  -> (l,v) : window n (tail xs)

takeR :: Int -> [a] -> [a]
takeR n l = go (drop n l) l
  where
    go [] r          = r
    go (_:xs) (_:ys) = go xs ys

takeS n _      | n <= 0 = S.empty
takeS _ []     = S.empty
takeS n (x:xs) = x S.<| takeS (n-1) xs

takeN :: Int -> [a] -> a -> [a] -> ([a],Maybe a)
takeN n  l z _     | n <= 0 = (l,Just z)
takeN _ l z []     = (l,Nothing)
takeN n l _ (x:xs) = takeN (n-1) (x:l) x xs
