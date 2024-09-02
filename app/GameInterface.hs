module GameInterface (displayBoard, getUserAction) where

import Minesweeper

displayBoard :: Board -> IO ()
displayBoard board = do
  putStrLn header
  mapM_ printRow (zip ['A' ..] board)
  where
    header = "   " ++ unwords [charToStr i | i <- [1 .. (length (head board))]]
    charToStr i = [toEnum (i + fromEnum 'A' - 1) :: Char]
    printRow (r, row) = putStr (charToNumberString r ++ " ") >> putStrLn (concatMap showCell row)
    showCell (Mine, Hidden) = "■ "
    showCell (Mine, Revealed) = "● "
    showCell (Number n, Hidden) = "■ "
    showCell (Number n, Revealed) = show n ++ " "
    showCell (_, Flagged) = "▸ "

charToNumberString :: Char -> String
charToNumberString c
  | num <= 9  = " " ++ show num
  | otherwise = show num
  where
    num = fromEnum c - fromEnum 'A' + 1

getUserAction :: IO (String, (Int, Int))
getUserAction = do
  putStrLn "Escolha uma ação: (D)esenterrar ou (B)andeira, seguido de coordenadas (ex: D A1):"
  input <- getLine
  let (action, coords) = splitAt 1 input
      pos = parseCoords coords
  return (action, pos)

parseCoords :: String -> (Int, Int)
parseCoords (blank : r : c) = (fromEnum r - fromEnum 'A', (read c :: Int) - 1)
parseCoords _ = (0, 0)
