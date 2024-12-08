module Lib
  ( DaySolution,
    Solution,
    safeParse,
    parseNumber,
    parseChar,
  )
where

import Data.Functor (($>))
import Text.Parsec (Parsec, anyChar, digit, many1, runParser)

type Filename = String

type FileContents = String

type Solution = (Filename, FileContents) -> String

type DaySolution = (Solution, Solution)

parseNumber :: Parsec String () Int
parseNumber = read <$> many1 digit

safeParse :: Parsec String state val -> state -> Filename -> FileContents -> val
safeParse p s fn fc = case runParser p s fn fc of
  Left err -> error $ show err
  Right v -> v

parseChar :: Parsec String b (Maybe a)
parseChar = anyChar $> Nothing