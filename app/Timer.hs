module Timer (startTimer, endTimer, currTimer) where

import System.Clock
    ( TimeSpec, diffTimeSpec, getTime, toNanoSecs, Clock(Monotonic) )

startTimer :: IO TimeSpec
startTimer = getTime Monotonic

currTimer :: TimeSpec -> IO ()
currTimer start = do
  curr <- getTime Monotonic
  putStrLn $ "\n>> Tempo decorrido: " ++ show (toNanoSecs (diffTimeSpec curr start) `div` (10 ^ 9)) ++ " segundos\n"

endTimer :: TimeSpec -> IO ()
endTimer start = do
  end <- getTime Monotonic
  putStrLn $ "\n>> Tempo decorrido: " ++ show (toNanoSecs (diffTimeSpec end start) `div` (10 ^ 9)) ++ " segundos."