-- Tipo de dados para representar uma célula no tabuleiro
data Cell = Empty | Mine | Number Int | Revealed Cell deriving (Show, Eq)

-- Tipo para representar o tabuleiro como uma lista de listas de células
type Board = [[Cell]]

-- Função para verificar se o jogador venceu
checkVictory :: Board -> Bool
checkVictory board = all isRevealedOrMine (concat board)
  where
    isRevealedOrMine :: Cell -> Bool
    isRevealedOrMine (Revealed (Number _)) = True
    isRevealedOrMine (Revealed Empty) = True
    isRevealedOrMine Mine = True
    isRevealedOrMine _ = False
