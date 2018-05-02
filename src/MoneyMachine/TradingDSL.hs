{-# LANGUAGE DeriveFunctor, FlexibleContexts #-}
module MoneyMachine.TradingDSL where

import           Control.Monad.Trans.Free
import           MoneyMachine.Order

data TradingDSL marketData openPositions closedPositions pendingOrder openOrder cancelPendingOrder error next
    = RunStrategy
        ((marketData,openPositions,closedPositions,[pendingOrder]) -> ([openOrder],[cancelPendingOrder]))
        (([openOrder],[cancelPendingOrder]) -> next)
    | OpenOrder openOrder next
    | CancelPendingOrder cancelPendingOrder next
    | Hold next
    | TradingThrow error next
    | PrintLine String next
    | TradingDone
    deriving Functor

runStrategy strategy = liftF (RunStrategy strategy id)
openOrder order = liftF (OpenOrder order ())
cancelPendingOrder order = liftF (CancelPendingOrder order ())
tradingThrow error = liftF (TradingThrow error ())
tradingPrintLine line = liftF (PrintLine line ())
