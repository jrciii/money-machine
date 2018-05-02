module MoneyMachine.MockTradeInterpreterSpec where

import qualified Data.Map as M
import Test.Hspec
import Test.QuickCheck
import Control.Monad.Trans.State.Lazy

import MoneyMachine.Order
import MoneyMachine.Trade
import MoneyMachine.MockTradeInterpreter

main :: IO ()
main = hspec spec

--   newtype MockStrategy = MockStrategy
--     { runMockStrategy :: TradingState -> StrategyResult
--     }
openMarketOrderStrategy :: MockStrategy
openMarketOrderStrategy (md, op, cp, po) =
    let (inst, _) = head $ M.toList md
    in ([MarketOrder { units = 1000, instrument = inst}],[])

shouldTrade = (modify (+1) >> get) >>= return . (==2)

marketData = M.fromList [("EUR_USD",[(1.0002,1.0000)])]

spec :: Spec
spec =
  describe "mockInterpret" $ do
    it "opens market orders from strategies" $
      execState
      (mockInterpret (trade openMarketOrderStrategy shouldTrade)) (marketData, M.fromList [], M.fromList [], [()])
      `shouldBe` (marketData,M.fromList [("EUR_USD",[(1000,1.0000)])], M.fromList [], [()])