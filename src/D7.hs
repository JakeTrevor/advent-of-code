module D7 (d7) where

import Lib (DaySolution, Solution, parseNumber, safeParse)
import Text.Parsec (Parsec, char, many1, sepBy1, spaces)

data Expression = MkExpr {exprTarget :: Int, runningTotal :: Int, sources :: [Int]}
  deriving (Show)

d7 :: DaySolution
d7 = (d7e1, d7e2)

parseExpr :: Parsec String () Expression
parseExpr =
  MkExpr
    <$> parseNumber
    <* char ':'
    <* spaces
    <*> parseNumber
    <* char ' '
    <*> sepBy1 parseNumber (char ' ')

parseExprs :: Parsec String () [Expression]
parseExprs = many1 (parseExpr <* spaces)

tryExpr :: Expression -> Bool
tryExpr MkExpr {exprTarget = t, runningTotal = v, sources = []} = t == v
tryExpr e@MkExpr {exprTarget = target, runningTotal = total, sources = (x : rest)} =
  (total <= target) && (tryMul || tryAdd)
  where
    tryMul = tryExpr e {runningTotal = total * x, sources = rest}
    tryAdd = tryExpr e {runningTotal = total + x, sources = rest}

d7e1 :: Solution
d7e1 (filename, contents) = ans
  where
    expressions = safeParse parseExprs () filename contents
    validExpressions = filter tryExpr expressions
    totals = map exprTarget validExpressions
    ans = show . sum $ totals

join :: Int -> Int -> Int
join a b = read $ show a ++ show b

tryExprWithConcat :: Expression -> Bool
tryExprWithConcat MkExpr {exprTarget = t, runningTotal = v, sources = []} = t == v
tryExprWithConcat e@MkExpr {exprTarget = target, runningTotal = total, sources = (x : rest)} =
  (total <= target) && (tryMul || tryAdd || tryConcat)
  where
    tryMul = tryExprWithConcat e {runningTotal = total * x, sources = rest}
    tryAdd = tryExprWithConcat e {runningTotal = total + x, sources = rest}
    tryConcat = tryExprWithConcat e {runningTotal = join total x, sources = rest}

d7e2 :: Solution
d7e2 (filename, contents) = ans
  where
    expressions = safeParse parseExprs () filename contents
    validExpressions = filter tryExprWithConcat expressions
    totals = map exprTarget validExpressions
    ans = show . sum $ totals