module A4 where

import A1
import A2
import A3 hiding (
  _HEADER_,
  showSquares,
  dropFirstCol,
  dropLastCol,
  formatRows,
  isWinningLine,
  prependRowIndices
  )

-- *** Assignment 4-1 *** --

-- Q#01

_HEADER_ = ' ' : formatLine (map show _RANGE_)

-- Q#02

showSquares :: [Square] -> [String]
showSquares squareList = map showSquare squareList

-- Q#03

dropFirstCol :: Board -> Board
dropFirstCol board = map tail board

-- Q#04

dropLastCol :: Board -> Board
dropLastCol board = map init board

--Q#05

formatRows :: [Row] -> [String]
formatRows rows = map (\x -> formatLine (showSquares x)) rows

-- Q#06

isWinningLine_ :: Player -> Line -> Bool
isWinningLine_ _ [] = False
isWinningLine_ player line = null (filter (/= player) line)


-- *** Assignment 4-2 *** --

-- Q#07

isWinningLine' :: Player -> Line -> Bool
isWinningLine' _ [] = False
isWinningLine' player line = foldr (\sq acc -> acc && sq == player) True line

-- Q#08

_X_WIN_ = [ [X, O, O]
          , [O, X, O]
          , [O, O, X]
          ]
          
_O_WIN_ = [ [O, X, O]
          , [X, X, O]
          , [X, O, O]
          ]

hasWon :: Player -> Board -> Bool
hasWon player board = foldr (\line acc -> acc || isWinningLine' player line) False (getAllLines board)

-- Q#09

getGameState :: Board -> GameState
getGameState board 
    | hasWon X board = XWon
    | hasWon O board = OWon
    | foldr (\row outerAcc -> foldr (\sq innerAcc -> innerAcc || sq == Neither) outerAcc row) False board = InProgress
    | otherwise = Tie


playMove :: Player -> Board -> Move -> (GameState, Board)
playMove player board move = (getGameState newboard, newboard)
    where
        newboard = putSquare player board move

-- Q#10

prependRowIndices :: [String] -> [String]
prependRowIndices input = zipWith (:) ['A' ..] input

-- Q#11

formatBoard :: Board -> String
formatBoard board = unlines . (_HEADER_:) . prependRowIndices . formatRows $ board