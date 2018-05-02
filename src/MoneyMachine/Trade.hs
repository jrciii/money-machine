module MoneyMachine.Trade where

import Control.Monad.Loops
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free
import MoneyMachine.TradingDSL

trade ::
     (Monad m)
  => ((marketData, openPositions, closedPositions, [pendingOrder]) -> ( [openOrder]
                                                                      , [cancelPendingOrder]))
  -> m Bool
  -> FreeT (TradingDSL marketData openPositions closedPositions pendingOrder openOrder cancelPendingOrder error) m ()
trade strategy shouldTrade = do
  x <- lift shouldTrade
  if x
    then do
      orders <- runStrategy strategy
      executeOrders orders
      trade strategy shouldTrade
    else pure ()

executeOrders ::
     (Monad m)
  => ([openOrder], [cancelPendingOrder])
  -> FreeT (TradingDSL marketData openPositions closedPositions pendingOrder openOrder cancelPendingOrder error) m ()
executeOrders (newOpenOrders, newCancelPendingOrders) = do
  mapM_ openOrder newOpenOrders
  mapM_ cancelPendingOrder newCancelPendingOrders