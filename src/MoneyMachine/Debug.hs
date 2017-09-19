module MoneyMachine.Debug where

import Debug.Trace

traceThis x = trace(show x) $ x