module GameLogic (dig, flag, checkVictory, checkDefeat) where

import Minesweeper

dig :: Board -> (Int, Int) -> Board
dig board (x, y) = revealCells board [(x, y)]

revealCells :: Board -> [(Int, Int)] -> Board
revealCells board [] = board
revealCells board ((x, y) : rest)
  | x < 0 || y < 0 || x >= length board || y >= length (head board) = revealCells board rest
  | otherwise = case board !! y !! x of
      (Mine, Hidden) -> revealCells (updateBoard board (x, y) (Mine, Revealed)) rest
      (Number 0, Hidden) -> revealCells (updateBoard board (x, y) (Number 0, Revealed)) (neighbors (x, y) ++ rest)
      (Number n, Hidden) -> revealCells (updateBoard board (x, y) (Number n, Revealed)) rest
      (Mine, Flagged) -> revealCells (updateBoard board (x, y) (Mine, Revealed)) rest
      (Number n, Flagged) -> revealCells (updateBoard board (x, y) (Number n, Revealed)) rest
      _ -> revealCells board rest

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [(x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], (dx, dy) /= (0, 0)]

flag :: Board -> (Int, Int) -> Board
flag board (x, y) = updateBoard board (x, y) (cell, Flagged)
  where
    (cell, _) = board !! y !! x

updateBoard :: Board -> (Int, Int) -> (Cell, State) -> Board
updateBoard board (x, y) val = take y board ++ [take x row ++ [val] ++ drop (x + 1) row] ++ drop (y + 1) board
  where
    row = board !! y

checkVictory :: Board -> Bool
checkVictory board = all revealedOrFlagged (concat board)
  where
    revealedOrFlagged (Mine, _) = True
    revealedOrFlagged (Number n, Revealed) = True
    revealedOrFlagged _ = False

checkDefeat :: Mode -> Board -> Bool
checkDefeat mode board =
  case mode of 
    Survival -> any mineRevealedOrNumberFlagged (concat board)
      where
        mineRevealedOrNumberFlagged (Mine, Revealed) = True
        mineRevealedOrNumberFlagged (Number n, Flagged) = True
        mineRevealedOrNumberFlagged _ = False
    _ -> any mineRevealed (concat board)
      where
        mineRevealed (Mine, Revealed) = True
        mineRevealed _ = False