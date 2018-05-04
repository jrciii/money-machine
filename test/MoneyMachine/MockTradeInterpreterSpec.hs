module MoneyMachine.MockTradeInterpreterSpec where

import Control.Monad.Trans.State.Lazy
import qualified Data.Map as M
import Test.Hspec
import Test.QuickCheck

import MoneyMachine.MockTradeInterpreter
import MoneyMachine.Order
import MoneyMachine.Trade

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "mockInterpret" $ do
    it "opens market orders from strategies" $ do
      let openMarketOrderStrategy (md, op, cp, po) =
            let (inst, _) = head $ M.toList md
            in ([MarketOrder {marketOrderUnits = 1000, marketOrderInstrument = inst}], [])
      let shouldTrade = fmap (/= 2) (modify (+ 1) >> get)
      let marketData = M.fromList [("EUR_USD", [(1.0002, 1.0000)])]
      let prog = trade openMarketOrderStrategy shouldTrade
      res <-
        (evalStateT
           (execStateT
              (mockInterpret id prog)
              (marketData, M.empty, M.empty, []))
           0)
      res `shouldBe`
        ( marketData
        , M.fromList [("EUR_USD", [(1000, 1.0000)])]
        , M.fromList [("EUR_USD", [])]
        , [])
