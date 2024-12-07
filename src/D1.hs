module D1 (d1) where

import Data.IntMap.Strict (IntMap, empty, findWithDefault, insertWith)
import Data.List (sort)
import Lib (DaySolution, Solution, parseNumber, safeParse)
import Text.Parsec (Parsec, many1, spaces)

d1 :: DaySolution
d1 = (d1e1, d1e2)

parseLine :: Parsec String () (Int, Int)
parseLine = do
  a <- parseNumber <* spaces
  b <- parseNumber <* spaces
  return (a, b)

parseLines :: Parsec String () [(Int, Int)]
parseLines = many1 parseLine

tuplesToLists :: [(Int, Int)] -> ([Int], [Int])
tuplesToLists [] = ([], [])
tuplesToLists ((a, b) : rest) = (a : as, b : bs)
  where
    (as, bs) = tuplesToLists rest

listToTuples :: ([Int], [Int]) -> [(Int, Int)]
listToTuples = uncurry zip

diff :: Int -> Int -> Int
diff x y = abs (x - y)

d1e1 :: Solution
d1e1 (filename, contents) = ans
  where
    result = safeParse parseLines () filename contents
    (left, right) = tuplesToLists result
    sorted = listToTuples (sort left, sort right)
    diffs = map (uncurry diff) sorted
    ans = show $ sum diffs

count :: [Int] -> IntMap Int
count = foldr (\x -> insertWith (+) x 1) empty

mulFreq :: IntMap Int -> Int -> Int
mulFreq freqMap key = findWithDefault 0 key freqMap * key

d1e2 :: Solution
d1e2 (filename, contents) = ans
  where
    result = safeParse parseLines () filename contents
    (left, right) = tuplesToLists result
    freqMap = count right
    left' = map (mulFreq freqMap) left
    ans = show $ sum left'