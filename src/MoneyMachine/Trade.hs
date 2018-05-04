module MoneyMachine.Trade where

import Control.Monad.Loops
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free
import MoneyMachine.TradingDSL

trade ::
     (Monad m)
  => ((marketData, openPositions, closedPositions, [pendingOrder]) -> ( [openOrder]
                                                                      , [pendingOrder]
                                                                      , [cancelPendingOrder]))
  -> m Bool
  -> FreeT (TradingDSL marketData openPositions closedPositions pendingOrder openOrder cancelPendingOrder error) m ()
trade strategy shouldTrade =
  whileM_ (lift shouldTrade) $ do
      orders <- runStrategy strategy
      executeOrders orders

executeOrders ::
     (Monad m)
  => ([openOrder], [pendingOrder], [cancelPendingOrder])
  -> FreeT (TradingDSL marketData openPositions closedPositions pendingOrder openOrder cancelPendingOrder error) m ()
executeOrders (newOpenOrders, newPendingOrders, newCancelPendingOrders) = do
  mapM_ openOrder newOpenOrders
  mapM_ openPendingOrder newPendingOrders
  mapM_ cancelPendingOrder newCancelPendingOrders