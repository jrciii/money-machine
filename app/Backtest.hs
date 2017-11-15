{-# LANGUAGE OverloadedStrings #-}
module Backtest where

import qualified Data.Map                       as M
import           Data.TALib
import           MoneyMachine.Backtest
import           MoneyMachine.ClusterStrategy
import           System.Environment
import           Text.Printf

main :: IO ()
main  = do
  c_ta_init
  cargs <- getArgs
  let files = words $ cargs !! 0
  let labels = words $ cargs !! 1
  let ws = read $ cargs !! 2
  profit <- backtest (clusterStrategy ws) files labels ws
  printf "%.5f\n" profit
