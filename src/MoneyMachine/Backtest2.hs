module MoneyMachine.Backtest2 where

import           Control.Monad
import qualified Data.List             as L
import           Data.Maybe
import           MoneyMachine.Candle
import           MoneyMachine.HistData
import           MoneyMachine.Order
import           MoneyMachine.Trade

backtest2 :: [String] -> [String] -> Int -> IO Double
backtest2 files instLabels ws = do
    contents <- mapM readFile files
    let lined = map lines contents
    let parsed = map (map histDataToCandle) lined
    let windowed = map (window ws) parsed
    let zipped = zip instLabels windowed
    -- mapM_ (putStrLn . show . (\(a,b) -> (a, take 3 b))) zipped
    return 0.0

checkOpenOrders :: [Order] -> [(String,[Candle])] -> ([Order],[Trade])
checkOpenOrders orders window =
  let checked = map (checkOrder window) orders
  in L.foldl' (\(openOrders,trades) m -> acc openOrders trades m) ([],[]) checked
  where acc orders trades (Left order)  = (orders ++ [order],trades)
        acc orders trades (Right trade) = (orders,trades ++ [trade])

checkOrder :: [(String,[Candle])] -> Order -> Either Order Trade
checkOrder window order =
    let candle = head . fromJust . L.lookup (_orderInstrument order) $ window
        Units units = _orderUnits order
        l = _l candle
        h = _h candle
        Price tp = fromJust . _takeProfit $ order
        Price sl = fromJust . _stopLoss $ order
        Price bp = _orderPrice order
        checkLong order candle | (l <= sl) = Just $ closeTrade (_orderUnits order) (_orderInstrument order) (_orderPrice order) (Price sl) (sl - bp)
                               | (h >= tp) = Just $ closeTrade (_orderUnits order) (_orderInstrument order) (_orderPrice order) (Price tp) (tp - bp)
                               | otherwise = Nothing
        checkShort order candle | (h >= sl) = Just $ closeTrade (_orderUnits order) (_orderInstrument order) (_orderPrice order) (Price sl) (bp - sl)
                                | (l <= tp) = Just $ closeTrade (_orderUnits order) (_orderInstrument order) (_orderPrice order) (Price tp) (bp - tp)
                                | otherwise = Nothing
        result = if (units > 0) then checkLong order candle else checkShort order candle
    in orderOrTrade order result

orderOrTrade :: Order -> Maybe Trade -> Either Order Trade
orderOrTrade order Nothing  = Left order
orderOrTrade _ (Just trade) = Right trade

closeTrade :: Units -> String -> Price -> Price -> Double -> Trade
closeTrade units inst open close profit = Trade {_tradeInstrument = inst, _tradeOpen = open, _tradeClose = close, _tradeUnits = units, _tradeProfit = Points profit}


window :: Int -> [a] -> [[a]]
window n xs =
  let taken = take n xs
  in
    if (length taken) < n then []
    else taken : window n (tail xs)
