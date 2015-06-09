{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches [] _ = 0
exactMatches _ [] = 0
exactMatches (x:xs) (y:ys) | x == y = 1 + exactMatches xs ys | otherwise = 0 + exactMatches xs ys

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors code = go [0,0,0,0,0,0] code
  where go :: [Int] -> Code -> [Int]
        go ax [] = ax
        go ax (x:xs)
          | x == Red = go ((head ax + 1) : (tail ax)) xs
          | x == Green = go ((take 1 ax) ++ ((ax!!1 + 1) : (drop 2 ax))) xs
          | x == Blue  = go ((take 2 ax) ++ ((ax!!2 + 1) : (drop 3 ax))) xs
          | x == Yellow = go ((take 3 ax) ++ ((ax!!3 + 1) : (drop 4 ax))) xs
          | x == Orange = go ((take 4 ax) ++ ((ax!!4 + 1) : (drop 5 ax))) xs
          | x == Purple = go ((take 5 ax) ++ ((ax!!5 + 1) : (drop 6 ax))) xs
          | otherwise = [0]

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches [] [] = 0
matches code1 code2 = go 0 (countColors code1) (countColors code2)
  where go :: Int -> [Int] -> [Int] -> Int
        go acc [] [] = acc
        go acc (x:xs) (y:ys)
          | x <= y = go (acc+x) xs ys
          | otherwise = go (acc+y) xs ys

--  where go :: [Int] -> [Int] -> Int
--        go [] [] = acc
--        go (x:xs) (y:ys)
--          | min x y go 

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove = undefined

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent = undefined

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes = undefined

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes = undefined

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve = undefined

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
