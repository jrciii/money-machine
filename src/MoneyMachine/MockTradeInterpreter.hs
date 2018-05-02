module MoneyMachine.MockTradeInterpreter where

import Control.Monad.Trans.Free
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified Data.Map as M
import Data.Maybe
import MoneyMachine.Order
import MoneyMachine.Trade
import MoneyMachine.TradingDSL

type Bid = Double

type Ask = Double

type Units = Int

type Price = Double

type Instrument = String

-- marketData position pendingOrder openOrder cancelPendingOrder error next
type MarketData = M.Map Instrument [(Bid, Ask)]

type OpenPositionEntry = (Units, Price)

type ClosedPositionEntry = (Units, Price, Price)

type OpenPositions = M.Map Instrument [OpenPositionEntry]

type ClosedPositions = M.Map Instrument [ClosedPositionEntry]

type CancelPendingOrder = ()

type PendingOrder = ()

type Error = String

type TradingState = (MarketData, OpenPositions, ClosedPositions, [PendingOrder])

type StrategyResult = ([OpenOrder], [CancelPendingOrder])

type MockStrategy = TradingState -> StrategyResult

-- marketData openPositions closedPositions pendingOrder openOrder cancelPendingOrder error
type TradingProgram
   = FreeT (TradingDSL MarketData OpenPositions ClosedPositions PendingOrder OpenOrder CancelPendingOrder Error) (State TradingState) ()

mockInterpret :: TradingProgram -> State TradingState ()
mockInterpret prog = do
  x <- runFreeT prog
  case x of
    Free (RunStrategy strategy g) -> do
      tradingState <- get
      let strategyResult = strategy tradingState
      mockInterpret (g strategyResult)
    Free (OpenOrder openOrder next) -> do
      mockPlaceOpenOrder openOrder
      mockInterpret next
    Free (CancelPendingOrder cancelOrder next) -> undefined -- TODO implement
    --Free (Hold next) -> mockInterpret next
    Free (TradingThrow error next) -> do

      mockInterpret next
    --Free TradingDone -> tradingDone
    Pure r -> return r

mockPlaceOpenOrder :: OpenOrder -> State TradingState ()
mockPlaceOpenOrder MarketOrder {instrument = i, units = u} = do
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
  let (unitsAfterClosing, leftOpen, newlyClosed) = foldr updateOpenPosition (units,[],[])  openPositions
      tradeDirection u = compare u 0
      closePrice = case compare units 0 of
        LT -> bid
        _ -> ask
      updateOpenPosition op@(opUnits,opPrice) (unitsLeft,leftOpen,newlyClosed)
        | unitsLeft == 0 || tradeDirection unitsLeft == tradeDirection opUnits =
            (0,op : leftOpen,newlyClosed)
        | abs unitsLeft >= abs opUnits =
            (unitsLeft + opUnits, leftOpen, (opUnits,opPrice,closePrice) : newlyClosed)
        | otherwise =
            (0, (unitsLeft + opUnits, opPrice) : leftOpen, (unitsLeft,opPrice,closePrice) : newlyClosed)
  in if unitsAfterClosing == 0
        then (leftOpen, newlyClosed)
        else ((unitsAfterClosing,closePrice) : leftOpen, newlyClosed)