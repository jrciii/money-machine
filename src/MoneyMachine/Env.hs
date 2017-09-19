module MoneyMachine.Env where

import           OANDA
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B

getPracticeEnv = do
    (token,aid) <- getAcctInfo "practiceinfo.txt"
    return (practiceAuth (AccessToken (B.pack token)),AccountID aid)

getAcctInfo path = do
    contents <- readFile path
    let l = lines contents
    return (l !! 0, l !! 1)
