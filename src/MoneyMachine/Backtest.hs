{-# LANGUAGE BangPatterns #-}
module MoneyMachine.Backtest where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.RWS.Strict
import           Data.Either
import qualified Data.HashMap.Strict            as HM
import qualified Data.List                      as L
import           Data.Maybe
import qualified Data.Sequence                  as S
import qualified Data.Text                      as T
import qualified Data.Text.IO                      as TIO
import           Data.Time

import           MoneyMachine.Candle
import           MoneyMachine.HistData
import           MoneyMachine.Order
import           MoneyMachine.Strategy
import           MoneyMachine.Trade
import qualified Data.ByteString.Char8 as C
import qualified Data.Sequence as S
data Env = Env
           { _spread     :: !Spread
           , _commission :: !Commission
           }

backtest :: Strategy -> [String] -> [String] -> Int -> IO Double
backtest strat files instLabels ws = do
    contents <- mapM C.readFile files
    let parser = parseTimeOrError True defaultTimeLocale "%d.%m.%Y %H:%M:%S.000" :: String -> LocalTime
    let lined = map (drop 1 . C.lines) contents
    let parsed = map (map $ Just . histDataToCandle parser) lined
    let windowed = map (window ws) parsed
    let zipped = map (\(a,b) -> map (\x -> (a,x)) b) $ zip instLabels windowed
    let transposed = L.transpose zipped
    let env = Env {_spread = Spread (Points 0.00015), _commission = Commission 0.0}
    --putStrLn $ show $ fromJust $ L.find ((/=1) . L.length)  $ ((map L.nub) . (map (map (map _candleTime))) . (map (map snd))) transposed
    --putStrLn $ show $ takeR 3 transposed
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
       newOrders <- liftIO $ (_onTick strat) market spread commission remainingOrders trades
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
        Price tp = fromJust . _takeProfit $ order
        Price sl = fromJust . _stopLoss $ order
        Price bp = _orderPrice order
        checkLong | (l <= sl) = Just $ closeTrade (_orderUnits order) (_orderInstrument order) (_orderPrice order) (Price sl) (sl - bp)
                  | (h >= tp) = Just $ closeTrade (_orderUnits order) (_orderInstrument order) (_orderPrice order) (Price tp) (tp - bp)
                  | otherwise = Nothing
        checkShort | (h >= sl) = Just $ closeTrade (_orderUnits order) (_orderInstrument order) (_orderPrice order) (Price sl) (bp - sl)
                   | (l <= tp) = Just $ closeTrade (_orderUnits order) (_orderInstrument order) (_orderPrice order) (Price tp) (bp - tp)
                   | otherwise = Nothing
        result = if (units > 0) then checkLong else checkShort
    in case candle of
        Nothing -> Left order
        _       -> orderOrTrade order result

orderOrTrade :: Order -> Maybe Trade -> Either Order Trade
orderOrTrade order Nothing  = Left order
orderOrTrade _ (Just trade) = Right trade

closeTrade :: Units -> String -> Price -> Price -> Double -> Trade
closeTrade units inst open close profit = Trade {_tradeInstrument = inst, _tradeOpen = open, _tradeClose = close, _tradeUnits = units, _tradeProfit = Points profit}

lazyFmapTuple f ~(x, y) = (x, f y)

window :: Int -> [a] -> [([a],a)]
window n xs =
  let (l,b) = takeN n [] undefined xs
  in
    case b of Nothing -> []
              Just v  -> (l,v) : (window n (tail xs))

takeR :: Int -> [a] -> [a]
takeR n l = go (drop n l) l
  where
    go [] r          = r
    go (_:xs) (_:ys) = go xs ys

takeS n _ | n <= 0 = S.empty
takeS _ [] = S.empty
takeS n (x:xs) = x S.<| takeS (n-1) xs

takeN :: Int -> [a] -> a -> [a] -> ([a],Maybe a)
takeN n  l z _ | n <= 0 = (l,Just z)
takeN _ l z [] = (l,Nothing)
takeN n l _ (x:xs) = takeN (n-1) (x:l) x xs