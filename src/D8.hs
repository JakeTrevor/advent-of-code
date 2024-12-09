{-# LANGUAGE TupleSections #-}

module D8 (d8) where

import Control.Monad (void)
import Data.List (intercalate, nub)
import Data.Map.Strict (Map, empty, foldr', insertWith)
import Lib (DaySolution, Solution, safeParse)
import Text.Parsec (Parsec, anyChar, char, choice, getPosition, getState, many, modifyState, sourceColumn, sourceLine, try, (<|>))

d8 :: DaySolution
d8 = (d8e1, d8e2)

type Coord = (Int, Int)

diff :: Coord -> Coord -> Coord
diff (a, b) (x, y) = (a - x, b - y)

add :: Coord -> Coord -> Coord
add (a, b) (x, y) = (a + x, b + y)

type AntennaMap = Map Char [Coord]

addAntenna :: Char -> Coord -> AntennaMap -> AntennaMap
addAntenna c pos = insertWith (++) c [pos]

parseDot :: Parsec String AntennaMap ()
parseDot = void (char '.' <|> char '\n')

getPos :: Parsec a b Coord
getPos = do
  pos <- getPosition
  let x = sourceColumn pos - 2
  let y = sourceLine pos - 1
  return (x, y)

parseLetter :: Parsec String AntennaMap ()
parseLetter = anyChar >>= \x -> getPos >>= modifyState . addAntenna x

parseMap :: Parsec String AntennaMap ()
parseMap = void . many $ choice [try parseDot, parseLetter]

parseMapGet :: Parsec String AntennaMap AntennaMap
parseMapGet = parseMap >> getState

---

pairUp :: [a] -> [(a, a)]
pairUp (a : rest) = map (a,) rest ++ pairUp rest
pairUp _ = []

makeNodes :: Coord -> Coord -> [Coord]
makeNodes a b = [aboveA, belowB]
  where
    aToB = b `diff` a
    aboveA = a `diff` aToB
    belowB = aToB `add` b

getNodes :: [Coord] -> [Coord]
getNodes antennas = nodes
  where
    pairs = pairUp antennas
    nodes = concatMap (uncurry makeNodes) pairs

getAllNodes :: AntennaMap -> [Coord]
getAllNodes = foldr' ((++) . getNodes) []

inBounds :: Int -> Coord -> Bool
inBounds upper (x, y) = lowOK && highOK && rightOK && leftOK
  where
    leftOK = x >= 0
    rightOK = x < upper
    lowOK = y >= 0
    highOK = y < upper

d8e1 :: Solution
d8e1 (filename, contents) = ans
  where
    lns = lines contents
    sideLength = length lns
    contents' = intercalate "\n" . reverse $ lns
    result = safeParse parseMapGet empty filename contents'
    nodes = nub $ getAllNodes result
    nodesOnMap = filter (inBounds sideLength) nodes
    ans = show $ length nodesOnMap

---

makeDelta :: Coord -> Coord -> [(Coord, Coord)]
makeDelta a b = [(a, bToA), (b, aToB)]
  where
    bToA = a `diff` b
    aToB = b `diff` a

makeDeltas :: [Coord] -> [(Coord, Coord)]
makeDeltas antenna = concatMap (uncurry makeDelta) pairs
  where
    pairs = pairUp antenna

extendNode :: Int -> Coord -> Coord -> [Coord]
extendNode limit src delta = if inBounds limit src then src : rest else []
  where
    newSrc = src `add` delta
    rest = extendNode limit newSrc delta

extendNodes :: Int -> [Coord] -> [Coord]
extendNodes limit antennas = nodes
  where
    deltas = makeDeltas antennas
    nodes = concatMap (uncurry $ extendNode limit) deltas

extendAllNodes :: Int -> AntennaMap -> [Coord]
extendAllNodes limit = foldr' ((++) . extendNodes limit) []

d8e2 :: Solution
d8e2 (filename, contents) = ans
  where
    lns = lines contents
    sideLength = length lns
    contents' = intercalate "\n" . reverse $ lns
    result = safeParse parseMapGet empty filename contents'
    nodes = nub $ extendAllNodes sideLength result
    ans = show $ length nodes