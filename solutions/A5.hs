module A5 where

import A1
import A2
import A3
import A4

import System.Random.Stateful (globalStdGen, uniformM)
import Control.Monad (when)

-- *** Assignment 5-1 *** --

-- Q#01

printBoard :: Board -> IO ()
printBoard board = putStrLn . formatBoard $ board

-- Q#02
_LOGO_PATH_ :: FilePath
_LOGO_PATH_ = "./assets/logo.txt"

printLogo :: IO ()
printLogo = readFile _LOGO_PATH_ >>= putStrLn

-- Q#03
_RANDOM_BOOL_ :: IO Bool
_RANDOM_BOOL_ = uniformM globalStdGen

firstPlayer :: IO Player
--firstPlayer = _RANDOM_BOOL_ >>= (\b -> return . getFirstPlayer $ b)
firstPlayer = _RANDOM_BOOL_ >>= return . getFirstPlayer

-- Q#04

getMove :: Board -> IO Move
getMove board = getLine >>= worker . stringToMove
    where
        worker :: Move -> IO Move
        worker move = if isValidMove board move
                      then return move
                      else putStrLn "Invalid move! Try again" >> getMove board
        

-- Q#05

play :: Board -> Player -> IO ()
play board player = when _DISPLAY_LOGO_ printLogo
    >> printBoard board
    >> putStrLn (promptPlayer player)
    >> getMove board
    >>= executeMove
    where 
        executeMove :: Move -> IO ()
        executeMove move = let (newState, newBoard) = playMove player board move
            in case newState of
                InProgress -> play newBoard (switchPlayer player)
                otherwise  -> printBoard newBoard >> putStrLn (showGameState newState)
               

-- *** Assignment 5-2 *** --

-- Q#07

printLogoDo :: IO ()
printLogoDo = do
    logo <- readFile _LOGO_PATH_
    putStrLn logo

-- Q#08

firstPlayerDo :: IO Player
firstPlayerDo = do
    fp <- _RANDOM_BOOL_ 
    return . getFirstPlayer $ fp

-- Q#09

getMoveDo :: Board -> IO Move
getMoveDo board = do
    input <- getLine
    let move = stringToMove input
        valid = isValidMove board move
    if valid 
        then return move
        else do
            putStrLn "Invalid move! Try again"
            getMoveDo board

-- Q#10

playDo :: Board -> Player -> IO ()
playDo board player = do
    when _DISPLAY_LOGO_ printLogo
    printBoard board
    putStrLn (promptPlayer player)
    move <- getMove board
    let (newState, newBoard) = playMove player board move
    if newState == InProgress
        then do
            playDo newBoard (switchPlayer player)
        else do
            printBoard newBoard
            putStrLn (showGameState newState)