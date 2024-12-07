{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module D5 (d5) where

import Data.IntMap.Strict (IntMap, empty, findWithDefault, insertWith)
import Lib (DaySolution, Solution, parseNumber, safeParse)
import Text.Parsec (Parsec, char, many1, sepBy1, spaces)

data Rule = MkRule {before :: Int, after :: Int}

instance Show Rule where
  show (MkRule l r) = show l ++ "|" ++ show r

d5 :: DaySolution
d5 = (d5e1, d5e2)

parseRule :: Parsec String () Rule
parseRule = MkRule <$> parseNumber <* char '|' <*> parseNumber

parseRules :: Parsec String () [Rule]
parseRules = many1 (parseRule <* char '\n')

parseList :: Parsec String () [Int]
parseList = sepBy1 parseNumber $ char ','

parseLists :: Parsec String () [[Int]]
parseLists = sepBy1 parseList $ char '\n'

parseFile :: Parsec String () ([Rule], [[Int]])
parseFile = do
  rules <- parseRules <* spaces
  lists <- parseLists
  return (rules, lists)

constructDict :: [Rule] -> IntMap [Int]
constructDict [] = empty
constructDict ((MkRule l r) : rest) = insertWith (++) r [l] $ constructDict rest

checkSolution :: IntMap [Int] -> [Int] -> Bool
checkSolution _ [] = True
checkSolution ruleDict (x : rest) = xOK && checkSolution ruleDict rest
  where
    xOK = all (`notElem` others) rest
    others = findWithDefault [] x ruleDict

getMiddle :: [Int] -> Int
getMiddle ls = ls !! (length ls `div` 2)

d5e1 :: Solution
d5e1 (filename, contents) = ans
  where
    (rules, lists) = safeParse parseFile () filename contents
    dict = constructDict rules
    okLists = filter (checkSolution dict) lists
    middles = map getMiddle okLists
    ans = show $ sum middles

choose :: IntMap [Int] -> [Int] -> [Int] -> (Int, [Int])
choose rules prev (x : rest) =
  if xOK
    then (x, prev ++ rest)
    else choose rules (x : prev) rest
  where
    constraints = findWithDefault [] x rules
    xOK = all (`notElem` constraints) (prev ++ rest)
choose _ _ [] = error "couldn't find an unconstrained value"

reorder :: IntMap [Int] -> [Int] -> [Int] -> [Int]
reorder _ acc [] = acc
reorder rules acc stack = reorder rules (next : acc) stack'
  where
    (next, stack') = choose rules [] stack

fix :: IntMap [Int] -> [[Int]] -> [[Int]]
fix _ [] = []
fix rules (l : ls) = fixup l : fix rules ls
  where
    fixup = reverse . reorder rules []

d5e2 :: Solution
d5e2 (filename, contents) = ans
  where
    (rules, lists) = safeParse parseFile () filename contents
    dict = constructDict rules
    badLists = filter (not . checkSolution dict) lists
    fixedLists = fix dict badLists
    middles = map getMiddle fixedLists
    ans = show $ sum middles