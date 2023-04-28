module A3 where

import A1
import A2

import Data.List (transpose, intercalate)

-- *** Assignment 3-1 ***

-- Q#01

showInts :: [Int] -> [String]
showInts [] = []
showInts (x:xs) = show x : showInts xs


_HEADER_OLD_ = formatLine (showInts _RANGE_)

-- Q#02

showSquaresOld :: [Square] -> [String]
showSquaresOld [] = []
showSquaresOld (x:xs) =  showSquare x : showSquaresOld xs

-- Q#03

formatRowsOld :: [Row] -> [String]
formatRowsOld [] = []
formatRowsOld (x:xs) = formatLine (showSquaresOld x) : formatRowsOld xs

-- Q#04

isColEmpty :: Row -> Int -> Bool
isColEmpty [] _ = False
isColEmpty (x:_) 0 = x == Neither
isColEmpty (_:xs) col = isColEmpty xs (col - 1)

-- Q#05

dropFirstColOld :: Board -> Board
dropFirstColOld [] = []
dropFirstColOld (x:xs) = tail x : dropFirstColOld xs

dropLastColOld :: Board -> Board
dropLastColOld [] = []
dropLastColOld (x:xs) = init x : dropLastColOld xs

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

putSquare :: Player -> Board -> Move -> Board
putSquare player board (rowIndex, colIndex) = go rowIndex board
  where 
    go :: Int -> [Row] -> [Row]
    go _ [] = []
    go 0 (row:rows) = (replaceSquareInRow player colIndex row) : rows
    go index (row:rows) = row : (go (index-1) rows)
    
-- alternate putSquare
{-
putSquare p (r : rs) (0, j) = (replaceSquareInRow p j r) : rs
putSquare p b@(r : rs) (i, j) =
  | i > 0 = r : putSquare p rs (i-1, j)
  | otherwise = b
putSquare _ [] _ = []
-}

-- Q#08

{-
prependRowIndices :: [String] -> [String]
prependRowIndices input = go (indexRowStrings input)
  where
    go :: [(Char, String)] -> [String]
    go [] = []
    go ((l,str):xs) = (l : str) : go xs
-}

-- Q#09

isWinningLine :: Player -> Line -> Bool
isWinningLine player line = go False line
  where
    go :: Bool -> Line -> Bool
    go accum [] = accum
    go _ (sq:sqs) = player == sq && go True sqs

-- Q#10

isValidMove :: Board -> Move -> Bool
isValidMove board (rowIndex, colIndex) = isMoveInBounds (rowIndex, colIndex) && go rowIndex board
  where
    go :: Int -> [Row] -> Bool
    go _ [] = False
    go 0 (row:_) = isColEmpty row colIndex
    go rowIndex (row:rows) = go (rowIndex - 1) rows