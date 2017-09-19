module MoneyMachine.Backtest where
import qualified Data.HashMap.Lazy          as HM
import qualified Data.List                  as L
import           Data.Maybe
import           Data.Time.LocalTime
import           Debug.Trace
import           MoneyMachine.Candle
import           MoneyMachine.Chart
import           MoneyMachine.MarketHistory
import           MoneyMachine.Order
import           MoneyMachine.Strategy
import           MoneyMachine.Trade

data BacktestResult = BacktestResult
  { }

backtest :: LocalTime -> [(String, [Maybe Candle])] -> Strategy -> Spread -> Commission -> IO Double
backtest startTime instruments strat spread commission =
  let windowed = makeWindows (_windowSize strat) instruments
      openOrdersAndTradesIO = L.foldl' (\(ioOrdersTrades) window -> checkOpenOrdersAndMakeNewOrders startTime ioOrdersTrades strat spread commission window) (return ([],[])) windowed
  in do
     (openOrders,trades) <- openOrdersAndTradesIO
     return $ L.foldl' (\profit trade -> profit + (traceThis((\(Points p) -> p) (_tradeProfit trade)) - ((\( Spread (Points s)) -> s) spread))) 0 trades

checkOpenOrdersAndMakeNewOrders :: LocalTime -> IO ([Order],[Trade]) -> Strategy -> Spread -> Commission -> [(String,[Maybe Candle])] -> IO ([Order],[Trade])
checkOpenOrdersAndMakeNewOrders startTime ioOrdersTrades strat spread commission window =
  do
      (orders,trades) <- ioOrdersTrades
      let (unclosedOrders,newTrades) = checkOpenOrders orders window
      let newIoOrders = (_onTick strat) startTime (HM.fromList window) spread commission unclosedOrders []
      newOrders <- newIoOrders
      return (unclosedOrders ++ newOrders,trades ++ newTrades)

checkOpenOrders :: [Order] -> [(String,[Maybe Candle])] -> ([Order],[Trade])
checkOpenOrders orders window =
  let checked = map (checkOrder window) orders
  in L.foldl' (\(openOrders,trades) m -> acc openOrders trades m) ([],[]) checked
  where acc orders trades (Left order)  = (orders ++ [order],trades)
        acc orders trades (Right trade) = (orders,trades ++ [trade])

checkOrder :: [(String,[Maybe Candle])] -> Order -> Either Order Trade
checkOrder window order =
    let candle = head . fromJust . L.lookup (_orderInstrument order) $ window
        units = (\(Units x) -> x) (_orderUnits order)
        result = if (units > 0) then checkLong order candle else checkShort order candle
    in orderOrTrade order result

orderOrTrade :: Order -> Maybe Trade -> Either Order Trade
orderOrTrade order Nothing  = Left order
orderOrTrade _ (Just trade) = Right trade

checkLong :: Order -> Maybe Candle -> Maybe Trade
checkLong order Nothing = Nothing
checkLong order (Just candle) =
  let l = _l candle
      h = _h candle
      Price tp = fromJust . _takeProfit $ order
      Price sl = fromJust . _stopLoss $ order
      Price bp = _orderPrice order
  in if (l <= sl) then Just $ closeTrade (_orderUnits order) (_orderInstrument order) (_orderPrice order) (Price sl) (sl - bp)
     else if (h >= tp) then Just $ closeTrade (_orderUnits order) (_orderInstrument order) (_orderPrice order) (Price tp) (tp - bp)
     else Nothing

checkShort :: Order -> Maybe Candle -> Maybe Trade
checkShort order Nothing = Nothing
checkShort order (Just candle) =
  let l = _l candle
      h = _h candle
      Price tp = fromJust . _takeProfit $ order
      Price sl = fromJust . _stopLoss $ order
      Price bp = _orderPrice order
  in if (h >= sl) then Just $ closeTrade (_orderUnits order) (_orderInstrument order) (_orderPrice order) (Price sl) (bp - sl)
     else if (l <= tp) then Just $ closeTrade (_orderUnits order) (_orderInstrument order) (_orderPrice order) (Price tp) (bp - tp)
     else Nothing

closeTrade :: Units -> String -> Price -> Price -> Double -> Trade
closeTrade units inst open close profit = Trade {_tradeInstrument = inst, _tradeOpen = open, _tradeClose = close, _tradeUnits = units, _tradeProfit = Points profit}

makeWindows :: Int -> [(String,[Maybe Candle])] -> [[(String,[Maybe Candle])]]
makeWindows _ i@((_,[]):xs) = []
makeWindows n xs =
  let split = makeWindow n xs
  in (fst split) : (makeWindows n (snd split))
  where
    makeWindow n xs = unzip $ map (\(k,v) -> let s = splitAt n v in ((k, fst s),(k, snd s))) xs


window :: Int -> [a] -> [[a]]
window n xs =
  let taken = take n xs
  in
    if (length taken) < n then []
    else taken : window n (tail xs)

traceThis x = trace (show x) $ x
