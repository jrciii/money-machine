module Backtest where

import           MoneyMachine.Backtest
import           System.Environment
import MoneyMachine.ClusterStrategy
import MoneyMachine.NoopStrategy

main = do
  cargs <- getArgs
  let files = words $ cargs !! 0
  let instLabels = words $ cargs !! 1
  let ws = read $ cargs !! 2
  profit <- backtest (clusterStrategy ws) files instLabels ws
  putStrLn $ show profit
