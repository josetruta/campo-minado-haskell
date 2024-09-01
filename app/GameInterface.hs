module GameInterface (displayBoard, getUserAction) where

import Minesweeper

displayBoard :: Board -> IO ()
displayBoard board = do
  putStrLn "   1  2  3  4  5  6  7  8"
  mapM_ printRow (zip ['A' ..] board)
  where
    printRow (r, row) = putStr (r : " ") >> putStrLn (concatMap showCell row)
    showCell (Mine, Hidden) = "■ "
    showCell (Mine, Revealed) = "* "
    showCell (Number n, Hidden) = "■ "
    showCell (Number n, Revealed) = show n ++ " "
    showCell (Empty, Hidden) = "■ "
    showCell (Empty, Revealed) = "0 "
    showCell (_, Flagged) = "F "

getUserAction :: IO (String, (Int, Int))
getUserAction = do
  putStrLn "Escolha uma ação: (D)esenterrar ou (B)andeira, seguido de coordenadas (ex: D A1):"
  input <- getLine
  let (action, coords) = splitAt 1 input
      pos = parseCoords coords
  return (action, pos)

parseCoords :: String -> (Int, Int)
parseCoords (blank : r : c) = ((read c :: Int) - 1, fromEnum r - fromEnum 'A')
parseCoords _ = (0, 0)
