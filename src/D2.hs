{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module D2 (d2) where

import Lib (DaySolution, Solution)

d2 :: DaySolution
d2 = (d2e1, d2e2)

diffOK :: Int -> Bool
diffOK diff = diff <= 3 && diff > 0

makeDiffs :: [Int] -> [Int]
makeDiffs (x : y : rest) = (x - y) : makeDiffs (y : rest)
makeDiffs _ = []

readingOK :: [Int] -> Bool
readingOK [] = True
readingOK ls = and allDiffsOK
  where
    allDiffsOK = map (diffOK . (* polarity)) diffs
    diffs@(d1 : _) = makeDiffs ls
    polarity = if d1 > 0 then 1 else -1

d2e1 :: Solution
d2e1 (_, contents) = ans
  where
    wordList = map words $ lines contents
    readings = map (map read) wordList
    okReadings = filter readingOK readings
    ans = show $ length okReadings

makeVariants :: [Int] -> [Int] -> [[Int]]
makeVariants _ [] = []
makeVariants acc (x : rest) = (reverse acc ++ rest) : makeVariants (x : acc) rest

anyOK :: [[Int]] -> Bool
anyOK = foldr ((||) . readingOK) False

d2e2 :: Solution
d2e2 (_, contents) = ans
  where
    wordList = map words $ lines contents
    readings = map (map read) wordList
    readingVariants = map (\x -> x : makeVariants [] x) readings
    okReadings = filter anyOK readingVariants
    ans = show $ length okReadings