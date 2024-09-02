module Main (main) where

import System.Clock ( TimeSpec, Clock (Monotonic), toNanoSecs, diffTimeSpec, getTime )

import Minesweeper
    ( Mode(..), Difficulty(..), Board, initializeBoard )
import GameInterface ( displayBoard, getUserAction )
import GameLogic ( dig, flag, checkVictory, checkDefeat )
import Timer ( startTimer, endTimer, currTimer )
import TextCards ( winsCard, losesCard, menuCard, levelCard, timesOverCard )

main :: IO ()
main = do
    menuCard
    modeInput <- getLine
    levelCard
    diffInput <- getLine

    let mode = parseMode modeInput
    let difficulty = parseDifficulty diffInput
    board <- initializeBoard difficulty

    startGame mode difficulty board

startGame :: Mode -> Difficulty -> Board -> IO ()
startGame mode difficulty board = do
    startTime <- startTimer
    case mode of
        Timed -> gameLoop mode board startTime (getTimerDuration difficulty)
        _ -> gameLoop mode board startTime 0

gameLoop :: Mode -> Board -> TimeSpec -> Int -> IO ()
gameLoop mode board startTime timerDuration = do
    displayBoard board
    (action, pos) <- getUserAction
    let newBoard = if action == "D" then dig board pos else flag board pos
    
    if checkVictory newBoard
        then do
            displayBoard newBoard
            winsCard
            endTimer startTime
        else if checkDefeat mode newBoard
            then do
                displayBoard newBoard
                losesCard
                endTimer startTime
            else case mode of 
                Timed ->
                    do
                    curr <- getTime Monotonic
                    let currTime = toNanoSecs (diffTimeSpec curr startTime) `div` (10 ^ 9)
                    if currTime >= toInteger timerDuration
                        then do
                            timesOverCard
                        else do
                            currTimer startTime
                            gameLoop mode newBoard startTime timerDuration
                _ -> gameLoop mode newBoard startTime timerDuration

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
getTimerDuration Easy = 100
getTimerDuration Medium = 200
getTimerDuration Hard = 360

