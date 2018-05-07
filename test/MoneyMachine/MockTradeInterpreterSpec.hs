module MoneyMachine.MockTradeInterpreterSpec where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Data.Either
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
            in ( [ MarketOrder
                   {marketOrderUnits = 1000, marketOrderInstrument = inst}
                 ]
               , []
               , [])
      let shouldTrade = lift $ fmap (/= 2) (modify (+ 1) >> get)
      let marketData = M.fromList [("EUR_USD", [(1.0000, 1.0002)])]
      let prog = trade openMarketOrderStrategy shouldTrade (modify id >> get)
      res <-
        evalStateT
          (evalStateT
             (execStateT (mockInterpret prog) (undefined, M.empty, M.empty, []))
             marketData)
          0
      res `shouldBe`
        ( marketData
        , M.fromList [("EUR_USD", [(1000, 1.0002)])]
        , M.fromList [("EUR_USD", [])]
        , [])
    it
      "closes a long position when sell limit of same number of units is triggered" $ do
      let openOneMarketLongAndOneTakeProfitStrategy (md, op, cp, po) =
            let (inst, _) = head $ M.toList md
                Right po = mkPendingOrder (-1000) 1.0003 inst SellLimit
            in if null op
                 then ( [ MarketOrder
                          { marketOrderUnits = 1000
                          , marketOrderInstrument = inst
                          }
                        ]
                      , [po]
                      , [])
                 else ([], [], [])
      let shouldTrade = lift $ fmap (/= 3) (modify (+ 1) >> get)
      let marketData = M.fromList [("EUR_USD", [(0.9997, 0.9999)])]
      let marketModifier =
            modify (fmap (fmap (\(b, a) -> (b + 0.0003, a + 0.0003)))) >> get
      let prog =
            trade
              openOneMarketLongAndOneTakeProfitStrategy
              shouldTrade
              marketModifier
      res <-
        evalStateT
          (evalStateT
             (execStateT (mockInterpret prog) (undefined, M.empty, M.empty, []))
             marketData)
          0
      res `shouldBe`
        ( M.fromList [("EUR_USD", [(1.0003, 1.0005)])]
        , M.fromList [("EUR_USD", [])]
        , M.fromList [("EUR_USD", [(1000, 1.0002, 1.0003)])]
        , [])
