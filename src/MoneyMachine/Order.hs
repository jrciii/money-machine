module MoneyMachine.Order where

data OpenOrder = MarketOrder
    { instrument :: String,
      units :: Int
    }
