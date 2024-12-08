{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module D3 (d3) where

import Data.Functor (($>))
import Data.Maybe (catMaybes)
import Lib (DaySolution, Solution, parseChar, safeParse)
import Text.Parsec (Parsec, char, choice, digit, many1, optionMaybe, string, try)

d3 :: DaySolution
d3 = (d3e1, d3e2)

data Instruction = Mul {mulA :: Int, mulB :: Int} | Do | Don't
  deriving (Show)

eval :: Instruction -> Int
eval (Mul a b) = a * b
eval _ = error "cannot eval do or don't instructions"

parse3Num :: Parsec String () Int
parse3Num = do
  d1 <- digit
  d2 <- optionMaybe digit
  d3 <- optionMaybe digit
  let res = d1 : catMaybes [d2, d3]
  return $ read res

parseMul :: Parsec String () (Maybe Instruction)
parseMul = do
  _ <- string "mul("
  d1 <- parse3Num
  _ <- char ','
  d2 <- parse3Num
  _ <- char ')'
  return . Just $ Mul d1 d2

parseDo :: Parsec String () (Maybe Instruction)
parseDo = string "do()" $> Just Do

parseDon't :: Parsec String () (Maybe Instruction)
parseDon't = string "don't()" $> Just Don't

parseMuls :: Parsec String () [Maybe Instruction]
parseMuls = many1 (choice [try parseMul, parseChar])

d3e1 :: Solution
d3e1 (filename, contents) = ans
  where
    result = catMaybes $ safeParse parseMuls () filename contents
    numbers = map eval result
    ans = show $ sum numbers

removeDisabled :: [Instruction] -> [Instruction]
removeDisabled [] = []
removeDisabled (Don't : rest) = dropping rest
removeDisabled (Do : rest) = removeDisabled rest
removeDisabled (x : rest) = x : removeDisabled rest

dropping :: [Instruction] -> [Instruction]
dropping [] = []
dropping (Do : rest) = removeDisabled rest
dropping (_ : rest) = dropping rest

parseInstructions :: Parsec String () [Maybe Instruction]
parseInstructions = many1 (choice [try parseMul, try parseDon't, try parseDo, parseChar])

d3e2 :: Solution
d3e2 (filename, contents) = ans
  where
    result = catMaybes $ safeParse parseInstructions () filename contents
    keepers = removeDisabled result
    numbers = map eval keepers
    ans = show $ sum numbers