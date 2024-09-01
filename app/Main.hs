module Main (main) where

-- import Lib
import System.Clock

import Minesweeper
import GameInterface
import GameLogic
import Timer

main :: IO ()
main = do
    putStrLn "Bem-vindo ao Campo Minado!"
    putStrLn "Escolha o modo de jogo: (C)lássico, (S)obrevivência, ou (T)empo:"
    modeInput <- getLine
    putStrLn "Escolha o nível de dificuldade: (F)ácil, (M)édio, ou (D)ifícil:"
    diffInput <- getLine

    let mode = parseMode modeInput
    let difficulty = parseDifficulty diffInput
    board <- initializeBoard difficulty

    case mode of
        Timed -> do
            startCountdown (getTimerDuration difficulty)
            startGame mode board
        _ -> startGame mode board

startGame :: Mode -> Board -> IO ()
startGame mode board = do
    startTime <- startTimer
    gameLoop mode board startTime

gameLoop :: Mode -> Board -> TimeSpec -> IO ()
gameLoop mode board startTime = do
    displayBoard board
    (action, pos) <- getUserAction
    let newBoard = if action == "D" then dig board pos else flag board pos

    if checkVictory newBoard
        then do
            displayBoard newBoard
            putStrLn "Você venceu!"
            endTimer startTime
        else if checkDefeat newBoard
            then do
                displayBoard newBoard
                putStrLn "Você perdeu!"
            else gameLoop mode newBoard startTime

parseMode :: String -> Mode
parseMode "C" = Classic
parseMode "S" = Survival
parseMode "T" = Timed
parseMode "c" = Classic
parseMode "s" = Survival
parseMode "t" = Timed
parseMode _ = Classic

parseDifficulty :: String -> Difficulty
parseDifficulty "F" = Easy
parseDifficulty "M" = Medium
parseDifficulty "D" = Hard
parseDifficulty "f" = Easy
parseDifficulty "m" = Medium
parseDifficulty "d" = Hard
parseDifficulty _ = Easy

getTimerDuration :: Difficulty -> Int
getTimerDuration Easy = 360
getTimerDuration Medium = 180
getTimerDuration Hard = 60

