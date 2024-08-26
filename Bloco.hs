type Board = [[Cell]]
data Cell = Empty | Mine | Number Int | Revealed Cell deriving (Show, Eq)

-- Função que revela um bloco e, se for vazio, revela adjacentes
reveal :: Board -> (Int, Int) -> Board
reveal board (x, y)
  | outOfBounds board (x, y) = board  -- Verifica se está fora dos limites
  | Revealed _ <- board !! x !! y = board  -- Já revelado, nada a fazer
  | Mine <- board !! x !! y = board  -- Se for mina, não faz nada
  | Number _ <- board !! x !! y = revealCell board (x, y)  -- Revela número
  | Empty <- board !! x !! y = revealAdjacent (revealCell board (x, y)) (x, y)  -- Revela vazios adjacentes
  | otherwise = board

-- Função para revelar uma célula específica
revealCell :: Board -> (Int, Int) -> Board
revealCell board (x, y) = 
    let (before, row:after) = splitAt x board
        (rowBefore, cell:rowAfter) = splitAt y row
    in before ++ [rowBefore ++ [Revealed cell] ++ rowAfter] ++ after

-- Função para revelar blocos adjacentes recursivamente
revealAdjacent :: Board -> (Int, Int) -> Board
revealAdjacent board (x, y) = foldl reveal board adjacents
  where
    adjacents = [(i, j) | i <- [x-1..x+1], j <- [y-1..y+1], (i, j) /= (x, y)]

-- Função para verificar se uma posição está fora dos limites do tabuleiro
outOfBounds :: Board -> (Int, Int) -> Bool
outOfBounds board (x, y) = x < 0 || y < 0 || x >= length board || y >= length (head board)
