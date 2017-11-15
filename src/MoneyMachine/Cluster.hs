{-# LANGUAGE OverloadedStrings #-}
module MoneyMachine.Cluster where

import OANDA
import MoneyMachine.Candle
import Data.Thyme.LocalTime
import Data.Maybe
import qualified Data.Text as T
import Data.KMeans
import qualified Data.List as L

candleAsList :: Candlestick -> [Double]
candleAsList c = [read $ T.unpack $ unPriceValue $ candlestickDataC $ fromJust $ candlestickBid c]

kmeansSRo :: [Candlestick] -> Int -> [Double]
kmeansSRo cs numlines = let priceVals = map (\x -> candleAsList x) cs
                            clusters = map L.concat $ kmeans numlines priceVals
                         in map (\x -> (sum x) / (L.genericLength x)) clusters

kmeansSR :: [Candle] -> (Candle -> Double) -> Int -> [Double]
kmeansSR cs cToD numlines = let priceVals = map (\x -> [cToD x]) cs
                                clusters = map L.concat $ kmeans numlines priceVals
                            in map avg clusters
                            where avg l = let (sum,count) = L.foldl' (\(s,c) x -> (s+x,c+1)) (0.0,0) l
                                          in (sum/count)

kmeansSRn :: [Double] -> Int -> [Double]
kmeansSRn cs numlines = let priceVals = map (\x -> [x]) cs
                            clusters = map L.concat $ kmeans numlines priceVals
                        in map avg clusters
                        where avg l = let (sum,count) = L.foldl' (\(s,c) x -> (s+x,c+1)) (0.0,0) l
                                    in (sum/count)