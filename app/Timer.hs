module Timer (startTimer, endTimer, startCountdown) where

import System.Clock
import Control.Concurrent

startTimer :: IO TimeSpec
startTimer = getTime Monotonic

endTimer :: TimeSpec -> IO ()
endTimer start = do
  end <- getTime Monotonic
  putStrLn $ "Tempo decorrido: " ++ show (toNanoSecs (diffTimeSpec end start) `div` (10 ^ 9)) ++ " segundos."

startCountdown :: Int -> IO ()
startCountdown seconds = do
  putStrLn $ "Você tem " ++ show seconds ++ " segundos para completar o jogo!"
  countdown seconds
  where
    countdown 0 = putStrLn "Tempo esgotado!"
    countdown n = do
      putStrLn $ show n ++ "..."
      threadDelay 1000000 -- Atraso de 1 segundo
      countdown (n - 1)