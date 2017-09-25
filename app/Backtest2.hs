module Backtest2 where

import           MoneyMachine.Backtest2
import           System.Environment

main = do
  cargs <- getArgs
  let files = words $ cargs !! 0
  let instLabels = words $ cargs !! 1
  let ws = read $ cargs !! 2
  profit <- backtest2 files instLabels ws
  putStrLn $ show profit
