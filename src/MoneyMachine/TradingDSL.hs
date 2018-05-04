{-# LANGUAGE DeriveFunctor, FlexibleContexts #-}
module MoneyMachine.TradingDSL where

import           Control.Monad.Trans.Free
import           MoneyMachine.Order

data TradingDSL marketData openPositions closedPositions pendingOrder openOrder cancelPendingOrder error next
    = RunStrategy
        ((marketData,openPositions,closedPositions,[pendingOrder]) -> ([openOrder],[pendingOrder],[cancelPendingOrder]))
        (([openOrder],[pendingOrder],[cancelPendingOrder]) -> next)
    | OpenOrder openOrder next
    | OpenPendingOrder pendingOrder next
    | CancelPendingOrder cancelPendingOrder next
    | Hold next
    | TradingThrow error next
    | PrintLine String next
    | TradingDone
    deriving Functor

runStrategy strategy = liftF (RunStrategy strategy id)
openOrder order = liftF (OpenOrder order ())
openPendingOrder pendingOrder = liftF (OpenPendingOrder pendingOrder ())
cancelPendingOrder order = liftF (CancelPendingOrder order ())
tradingThrow error = liftF (TradingThrow error ())
tradingPrintLine line = liftF (PrintLine line ())
