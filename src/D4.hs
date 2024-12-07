module D4 (d4) where

import Data.Functor (($>))
import Data.List (intercalate, transpose)
import Data.Maybe (catMaybes)
import Lib (DaySolution, Solution, safeParse)
import Text.Parsec (Parsec, anyChar, choice, many1, string, try)

d4 :: DaySolution
d4 = (d4e1, d4e2)

parseXMAS :: Parsec String () (Maybe ())
parseXMAS = string "XMAS" $> Just ()

parseChar :: Parsec String () (Maybe a)
parseChar = anyChar $> Nothing

parseXmases :: Parsec String () [Maybe ()]
parseXmases = many1 (choice [try parseXMAS, parseChar])

diagonals :: [[a]] -> [[a]]
diagonals [] = []
diagonals ([] : xss) = xss
diagonals xss =
  zipWith
    (++)
    (map ((: []) . head) xss ++ repeat [])
    ([] : diagonals (map tail xss))

makeVariations :: String -> String
makeVariations str = intercalate "$" (map reverse allVariants ++ allVariants)
  where
    trans = intercalate "\n" . transpose . lines
    diags = intercalate "\n" . diagonals . lines
    revDiags = intercalate "\n" . diagonals . map reverse . lines
    allVariants = [str, trans str, diags str, revDiags str]

d4e1 :: Solution
d4e1 (filename, contents) = ans
  where
    variations = makeVariations contents
    result = catMaybes $ safeParse parseXmases () filename variations
    ans = show $ length result

match :: [[Char]] -> Bool
match [['M', _, 'M'], [_, 'A', _], ['S', _, 'S']] = True
match _ = False

makeBoxesLine :: [Char] -> [Char] -> [Char] -> [[[Char]]]
makeBoxesLine xs@(x1 : x2 : x3 : _) ys@(y1 : y2 : y3 : _) zs@(z1 : z2 : z3 : _) = variants ++ rest
  where
    box = [[x1, x2, x3], [y1, y2, y3], [z1, z2, z3]]
    variants = [box, transpose box, reverse box, (reverse . transpose) box]
    rest = makeBoxesLine (tail xs) (tail ys) (tail zs)
makeBoxesLine _ _ _ = []

makeBoxes :: [[Char]] -> [[[Char]]]
makeBoxes (xs : ys : zs : rest) = makeBoxesLine xs ys zs ++ makeBoxes (ys : zs : rest)
makeBoxes _ = []

d4e2 :: Solution
d4e2 (_, contents) = ans
  where
    boxes = makeBoxes $ lines contents
    result = filter match boxes
    ans = show $ length result