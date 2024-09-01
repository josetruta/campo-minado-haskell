module Minesweeper (initializeBoard, Difficulty (..), Mode (..), Board, Cell (..), State (..)) where

import System.Random (randomRIO)

data Cell = Mine | Number Int deriving (Show, Eq)

data State = Hidden | Revealed | Flagged deriving (Show, Eq)

type Board = [[(Cell, State)]]

data Difficulty = Easy | Medium | Hard deriving (Show, Eq)

data Mode = Classic | Survival | Timed deriving (Show, Eq)

initializeBoard :: Difficulty -> IO Board
initializeBoard difficulty = do
  let size = getSize difficulty
  mines <- generateMines size
  return $ placeMines size mines

getSize :: Difficulty -> Int
getSize Easy = 8
getSize Medium = 16
getSize Hard = 24

generateMines :: Int -> IO [(Int, Int)]
generateMines size = do
  let numMines = size * size `div` 6
  sequence [(,) <$> randomRIO (0, size - 1) <*> randomRIO (0, size - 1) | _ <- [1 .. numMines]]

placeMines :: Int -> [(Int, Int)] -> Board
placeMines size mines = [[if (x, y) `elem` mines then (Mine, Hidden) else getNumberState (x, y) mines | x <- [0 .. size - 1]] | y <- [0 .. size - 1]]

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [(x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], (dx, dy) /= (0, 0)]

count :: [(Int, Int)] -> [(Int, Int)] -> Int
count [] mines = 0
count ((x, y):rest) mines = if (x, y) `elem` mines then 1 + count rest mines else count rest mines

getNumberState :: (Int, Int) -> [(Int, Int)] -> (Cell, State)
getNumberState (x, y) mines = (Number n, Hidden)
  where n = count (neighbors (x, y)) mines
