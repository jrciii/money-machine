module MoneyMachine.TechnicalAnalysis where

import Data.List (scanl')

mavg :: Fractional b => Int -> [b] -> [b]
mavg k lst = map (/ fromIntegral k) $ scanl' (+) (sum h) $ zipWith (-) t lst
  where (h, t) = splitAt k lst
