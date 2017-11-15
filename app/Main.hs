module Main where
import           MoneyMachine.ClusterStrategy
import           MoneyMachine.Env
import           MoneyMachine.Practice
import           OANDA
import           System.Environment
import           System.IO

main = do
  hSetBuffering stdout LineBuffering
  cargs <- getArgs
  let granularity = getGran $ cargs !! 0
  let instName = cargs !! 1
  let instName2 = cargs !! 2
  let ws = read $ cargs !! 3
  (env,aid) <- getPracticeEnv
  practice env aid granularity [instName,instName2] (clusterStrategy ws)

getGran :: String -> CandlestickGranularity
getGran "S5"  = S5
getGran "S10" = S10
getGran "S15" = S15
getGran "S30" = S30
getGran "M1"  = M1
getGran "M2"  = M2
getGran "M4"  = M4
getGran "M5"  = M5
getGran "M10" = M10
getGran "M15" = M15
getGran "M30" = M30
getGran "H1"  = H1
getGran "H2"  = H2
getGran "H3"  = H3
getGran "H4"  = H4
getGran "H6"  = H6
getGran "H8"  = H8
getGran "H12" = H12
getGran "D"   = D
getGran "W"   = W
getGran "M"   = M
