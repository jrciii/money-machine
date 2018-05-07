module MoneyMachine.Order
  ( OpenOrder(..)
  , PendingOrderType(..)
  , PendingOrder
  , mkPendingOrder
  , getPendingOrderInfo
  ) where

data OpenOrder = MarketOrder
  { marketOrderInstrument :: String
  , marketOrderUnits :: Int
  } deriving Show

data PendingOrderType
  = BuyLimit
  | BuyStop
  | SellLimit
  | SellStop
  deriving (Show, Eq)

data PendingOrder = PendingOrder
  { pendingOrderInstrument :: String
  , pendingOrderUnits :: Int
  , triggerPrice :: Double
  , pendingOrderType :: PendingOrderType
  } deriving (Show, Eq)

getPendingOrderInfo :: PendingOrder -> (String,Int,Double,PendingOrderType)
getPendingOrderInfo (PendingOrder i u tp ty) = (i, u, tp, ty)

mkPendingOrder ::
     Int -> Double -> String -> PendingOrderType -> Either String PendingOrder
mkPendingOrder units triggerPrice instrument orderType =
  case orderType of
    BuyLimit
      | units > 0 ->
        Right
          PendingOrder
            { pendingOrderInstrument = instrument
            , pendingOrderUnits = units
            , triggerPrice = triggerPrice
            , pendingOrderType = orderType
            }
      | otherwise -> Left "Units must be greater than 0"
    BuyStop
      | units > 0 ->
        Right
          PendingOrder
              { pendingOrderInstrument = instrument
              , pendingOrderUnits = units
              , triggerPrice = triggerPrice
              , pendingOrderType = orderType
              }
      | otherwise -> Left "Units must be greater than 0"
    SellLimit
      | units < 0 ->
        Right
          PendingOrder
              { pendingOrderInstrument = instrument
              , pendingOrderUnits = units
              , triggerPrice = triggerPrice
              , pendingOrderType = orderType
              }
      | otherwise -> Left "Units must be less than 0"
    SellStop
      | units < 0 ->
        Right
          PendingOrder
              { pendingOrderInstrument = instrument
              , pendingOrderUnits = units
              , triggerPrice = triggerPrice
              , pendingOrderType = orderType
              }
      | otherwise -> Left "Units must be less than 0"
