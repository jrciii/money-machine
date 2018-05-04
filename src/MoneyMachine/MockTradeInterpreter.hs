module MoneyMachine.MockTradeInterpreter where

import Control.Lens
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import qualified Data.Map as M
import Data.Maybe
import Debug.Trace
import MoneyMachine.Order
import MoneyMachine.Trade
import MoneyMachine.TradingDSL

type Bid = Double

type Ask = Double

type Units = Int

type Price = Double

type Instrument = String

type MarketData = M.Map Instrument [(Bid, Ask)]

type OpenPositionEntry = (Units, Price)

type ClosedPositionEntry = (Units, Price, Price)

type OpenPositions = M.Map Instrument [OpenPositionEntry]

type ClosedPositions = M.Map Instrument [ClosedPositionEntry]

type CancelPendingOrder = ()

type Error = String

type TradingState = (MarketData, OpenPositions, ClosedPositions, [PendingOrder])

type StrategyResult = ([OpenOrder], [CancelPendingOrder])

type MockStrategy = TradingState -> StrategyResult

newtype MarketState = MarketState
  { getMarketState :: M.Map Instrument (Bid, Ask)
  }

mockInterpret ::
     (Monad m)
  => (MarketData -> MarketData)
  -> FreeT (TradingDSL MarketData OpenPositions ClosedPositions PendingOrder OpenOrder CancelPendingOrder Error) m ()
  -> StateT TradingState m ()
mockInterpret market prog = do
  (md, openPositions, closedPositions, pendingOrders) <- get
  let newMarketData = market md
  let (newOpenOrders, remainingPendingOrders) =
        checkPendingOrdersAgainstMarket newMarketData pendingOrders
  put (md, openPositions, closedPositions, remainingPendingOrders)
  mapM_ mockPlaceOpenOrder newOpenOrders
  x <- lift $ runFreeT prog
  case x of
    Free (RunStrategy strategy g) -> do
      tradingState <- get
      let strategyResult = strategy tradingState
      mockInterpret market (g strategyResult)
    Free (OpenOrder openOrder next) -> do
      mockPlaceOpenOrder openOrder
      mockInterpret market next
    Free (CancelPendingOrder cancelOrder next) -> undefined -- TODO implement
    --Free (Hold next) -> mockInterpret next
    Free (TradingThrow error next) -> do
      mockInterpret market next
    --Free TradingDone -> tradingDone
    Pure r -> return r

mockPlaceOpenOrder :: (Monad m) => OpenOrder -> StateT TradingState m ()
mockPlaceOpenOrder MarketOrder {marketOrderInstrument = i, marketOrderUnits = u} = do
  let direction = compare u 0
  (md, openPositions, closedPositions, pendingOrders) <- get
  let (bid, ask) = head $ fromJust $ M.lookup i md
  let openEntries = M.findWithDefault [] i openPositions
  let closedEntries = M.findWithDefault [] i closedPositions
  let (updatedOpenPositions, updatedClosedPositions) =
        updatePositions u bid ask openEntries closedEntries
  let newOpenPositions = M.insert i updatedOpenPositions openPositions
  let newClosedPositions = M.insert i updatedClosedPositions closedPositions
  put (md, newOpenPositions, newClosedPositions, pendingOrders)
mockPlaceOpenOrder _ = pure () -- TODO implement opening other types of orders

updatePositions ::
     Units
  -> Bid
  -> Ask
  -> [OpenPositionEntry]
  -> [ClosedPositionEntry]
  -> ([OpenPositionEntry], [ClosedPositionEntry])
updatePositions units bid ask openPositions closedPositions =
  let (unitsAfterClosing, leftOpen, newlyClosed) =
        foldr updateOpenPosition (units, [], []) openPositions
      tradeDirection u = compare u 0
      closePrice =
        case compare units 0 of
          LT -> bid
          _ -> ask
      updateOpenPosition op@(opUnits, opPrice) (unitsLeft, leftOpen, newlyClosed)
        | unitsLeft == 0 || tradeDirection unitsLeft == tradeDirection opUnits =
          (0, op : leftOpen, newlyClosed)
        | abs unitsLeft >= abs opUnits =
          ( unitsLeft + opUnits
          , leftOpen
          , (opUnits, opPrice, closePrice) : newlyClosed)
        | otherwise =
          ( 0
          , (unitsLeft + opUnits, opPrice) : leftOpen
          , (unitsLeft, opPrice, closePrice) : newlyClosed)
  in if unitsAfterClosing == 0
       then (leftOpen, newlyClosed)
       else ((unitsAfterClosing, closePrice) : leftOpen, newlyClosed)

checkPendingOrdersAgainstMarket ::
     MarketData -> [PendingOrder] -> ([OpenOrder], [PendingOrder])
checkPendingOrdersAgainstMarket marketData pendingOrders =
  foldr
    (\po (os, pos) ->
       maybe
         (os, po : pos)
         (\oe -> (oe : os, pos))
         (checkPendingOrderAgainstMarket marketData po))
    ([], [])
    pendingOrders

checkPendingOrderAgainstMarket :: MarketData -> PendingOrder -> Maybe OpenOrder
checkPendingOrderAgainstMarket marketData po =
  let (i, u, tp, ot) = getPendingOrderInfo po
      (bid, ask) = head $ fromJust $ M.lookup i marketData
  in case ot of
       BuyStop
         | ask >= tp ->
           Just MarketOrder {marketOrderInstrument = i, marketOrderUnits = u}
         | otherwise -> Nothing
       BuyLimit
         | ask <= tp ->
           Just MarketOrder {marketOrderInstrument = i, marketOrderUnits = u}
         | otherwise -> Nothing
       SellStop
         | bid <= tp ->
           Just MarketOrder {marketOrderInstrument = i, marketOrderUnits = u}
         | otherwise -> Nothing
       SellLimit
         | bid >= tp ->
           Just MarketOrder {marketOrderInstrument = i, marketOrderUnits = u}
         | otherwise -> Nothing
