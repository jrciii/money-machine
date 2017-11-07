{-# LANGUAGE OverloadedStrings #-}
module Backtest where

import           Data.KdTree.Static
import qualified Data.List                      as L
import qualified Data.Map                       as M
import           Data.TALib
import qualified Data.Text                      as T
import qualified Data.Thyme.Time                as THT
import qualified Data.Thyme.Time.Core           as THC
import           Data.Time
import           Data.Time.Clock.POSIX
import           Debug.Trace
import           MoneyMachine.Backtest
import           MoneyMachine.Candle
import           MoneyMachine.Chart
import           MoneyMachine.Cluster
import           MoneyMachine.ClusterStrategy
import           MoneyMachine.Env
import           MoneyMachine.Order
import           MoneyMachine.Practice
import           MoneyMachine.Strategy
import           MoneyMachine.TechnicalAnalysis
import           OANDA
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
