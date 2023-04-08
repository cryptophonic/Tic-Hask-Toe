{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module A2 where

import A1
import Data.List (intercalate)

-- *** Assignment 2-1 *** --

-- Q#01

promptPlayer :: Player -> String
promptPlayer Neither = error "Invalid player"
promptPlayer p = concat ["Player ", showSquare p, "'s turn: enter a row and column position (ex. A1)"]

-- Q#02

_RANGE_ = [0 .. _SIZE_-1]

-- Q#03

isDigit :: Char -> Bool
isDigit d = elem d ['0'..'9']

readDigit :: Char -> Int
readDigit d = if isDigit d
              then read [d]
              else -1

-- Q#04

_EMPTY_ROW_ = replicate 3 Neither

_EMPTY_BOARD_ = replicate 3 _EMPTY_ROW_

-- Q#05

isTied :: [[Square]] -> Bool
isTied board = if elem Neither (concat board)
               then False
               else True

_TIED_BOARD_ = [
    [X, O, O], [O, X, X], [O, X, O] 
  ]

-- Q#06

indexRowStrings :: [String] -> [(Char, String)]
indexRowStrings list = zip ['A' ..] list

-- Q#07

formatLine :: [String] -> String
formatLine list = _SEP_ ++ intercalate _SEP_ list ++ _SEP_

-- *** Assignment 2-2 *** --

-- Q#08

isMoveInBounds :: Move -> Bool
isMoveInBounds (row, col) = and [row >= 0, row < _SIZE_, col >= 0, col < _SIZE_]

-- Q#09

stringToMove :: String -> Move
stringToMove [row, col] = (convertRowIndex row, readDigit col)
stringToMove _ = _INVALID_MOVE_

-- Q#10

replaceSquareInRow :: Player -> Int -> Row -> Row
replaceSquareInRow player index row
    | or [ index < 0, index >= _SIZE_ ] = row
    | null row = []
    | otherwise = let (pre, _ : post) = splitAt index row
        in concat [ pre, [player], post ]

rsO = replaceSquareInRow O
rsX = replaceSquareInRow X
