module A3 where

import A1
import A2

import Data.List (transpose, intercalate)

-- *** Assignment 3-1 ***

-- Q#01

showInts :: [Int] -> [String]
showInts [] = []
showInts (x:xs) = show x : showInts xs


_HEADER_ = formatLine (showInts _RANGE_)

-- Q#02

showSquares :: [Square] -> [String]
showSquares [] = []
showSquares (x:xs) =  showSquare x : showSquares xs

-- Q#03

formatRows :: [Row] -> [String]
formatRows [] = []
formatRows (x:xs) = formatLine (showSquares x) : formatRows xs

-- Q#04

isColEmpty :: Row -> Int -> Bool
isColEmpty [] _ = False
isColEmpty (x:_) 0 = x == Neither
isColEmpty (_:xs) col = isColEmpty xs (col - 1)

-- Q#05

dropFirstCol :: Board -> Board
dropFirstCol [] = []
dropFirstCol (x:xs) = tail x : dropFirstCol xs

dropLastCol :: Board -> Board
dropLastCol [] = []
dropLastCol (x:xs) = init x : dropLastCol xs

-- Q#06

getDiag1 :: Board -> Row
getDiag1 board = go 0 board
  where
    go :: Int -> Board -> Row
    go _ [] = []
    go n (x:xs) = x !! n : go (n + 1) xs


getDiag2 :: Board -> Row
getDiag2 board = go 2 board
  where
    go :: Int -> Board -> Row
    go _ [] = []
    go n (x:xs) = x !! n : go (n - 1) xs


getAllLines :: Board -> [Row]
getAllLines board = board ++ (transpose board) ++ [ getDiag1 board, getDiag2 board ]

-- Create new test board because _TIED_BOARD_ is a transpose of itself, not good for testing
_TEST_BOARD_ = [
    [O, X, X], [X, X, O], [O, O, X]
  ]

-- *** Assignment 3-2 ***

-- Q#07

putSquare = undefined

-- Q#08

prependRowIndices = undefined

-- Q#09

isWinningLine = undefined

-- Q#10

isValidMove = undefined