module D10 (d10) where

import Data.List (intercalate, nub, partition)
import Data.Map.Strict (Map, empty, insert, (!), (!?))
import Lib (DaySolution, Solution)

d10 :: DaySolution
d10 = (d10e1, d10e2)

type Coord = (Int, Int)

type TerrainMap = Map Coord Int

data GEdge = Edge {from :: Coord, to :: Coord}
  deriving (Show, Eq)

showGEdge :: TerrainMap -> GEdge -> String
showGEdge tMap (Edge {from = f, to = t}) = show (tMap ! f) ++ "@" ++ show f ++ " -> " ++ show (tMap ! t) ++ "@" ++ show t

type Graph = [GEdge]

showGraph :: TerrainMap -> [GEdge] -> [Char]
showGraph tMap graph = intercalate "\n" $ map (showGEdge tMap) graph

ints :: String -> [Int]
ints = map (read . (: []))

makeTerrainMap :: Int -> [String] -> TerrainMap
makeTerrainMap _ [] = empty
makeTerrainMap y (row : rest) = foldl insertPos prev row'
  where
    prev = makeTerrainMap (y - 1) rest
    row' = zip (ints row) [0 ..]
    insertPos acc (height, x) = insert (x, y) height acc

neighbors :: Int -> Int -> [Coord]
neighbors x y =
  [ (x + 1, y),
    (x - 1, y),
    (x, y + 1),
    (x, y - 1)
  ]

diff :: Maybe Int -> Maybe Int -> Int
diff (Just a) (Just b) = b - a
diff _ _ = 12

edgeExists :: TerrainMap -> Coord -> Coord -> Bool
edgeExists _ _ (-1, _) = False
edgeExists _ _ (_, -1) = False
edgeExists tMap a b = diff (tMap !? a) (tMap !? b) == 1

makeGraph :: TerrainMap -> Int -> Graph
makeGraph tMap sideLength =
  [ Edge {from = (x, y), to = t}
    | x <- [0 .. sideLength],
      y <- [0 .. sideLength],
      t <- neighbors x y,
      edgeExists tMap (x, y) t
  ]

sourceLevel :: TerrainMap -> Int -> GEdge -> Bool
sourceLevel tMap x (Edge {from = t}) = tMap !? t == Just x

targetLevel :: TerrainMap -> Int -> GEdge -> Bool
targetLevel tMap x (Edge {to = t}) = tMap !? t == Just x

isPreEdge :: GEdge -> GEdge -> Bool
isPreEdge a b = to a == from b

join :: GEdge -> GEdge -> GEdge
join (Edge {from = f}) (Edge {to = t}) = Edge {from = f, to = t}

mergeDown :: TerrainMap -> Int -> Graph -> Graph
mergeDown tMap x edges = ans
  where
    (postEdges, others) = partition (sourceLevel tMap x) edges
    (preEdges, others') = partition isPreEdge' others
    isPreEdge' e = any (isPreEdge e) postEdges
    joinedEdges = [join i j | i <- preEdges, j <- postEdges, i `isPreEdge` j]
    ans = joinedEdges ++ others'

d10e1 :: Solution
d10e1 (_, contents) = ans
  where
    lns = lines contents
    len = length lns - 1
    tMap = makeTerrainMap len lns
    graph = makeGraph tMap len
    graph' = foldr (mergeDown tMap) graph (reverse [1 .. 8])
    justTrails = filter (targetLevel tMap 9) graph'
    ans = show . length . nub $ justTrails

d10e2 :: Solution
d10e2 (_, contents) = ans
  where
    lns = lines contents
    len = length lns - 1
    tMap = makeTerrainMap len lns
    graph = makeGraph tMap len
    graph' = foldr (mergeDown tMap) graph (reverse [1 .. 8])
    justTrails = filter (targetLevel tMap 9) graph'
    ans = show . length $ justTrails