module D6 (d6) where

import Control.Monad
import Data.List (intercalate, nub)
import Data.Set (Set, empty, insert, map, member, notMember, size, toList)
import Lib (DaySolution, Solution, parseChar, safeParse)
import Text.Parsec (Parsec, char, choice, getPosition, getState, many1, modifyState, sourceColumn, sourceLine, try)

d6 :: DaySolution
d6 = (d6e1, d6e2)

type Coord = (Int, Int)

data Direction = DRight | DLeft | DUp | DDown
  deriving (Show, Eq, Ord)

turn :: Direction -> Direction
turn DRight = DDown
turn DDown = DLeft
turn DLeft = DUp
turn DUp = DRight

data Room = MkRoom
  { position :: (Coord, Direction),
    obstacles :: Set Coord,
    visited :: Set (Coord, Direction),
    sideLength :: Int
  }
  deriving (Show)

emptyRoom :: Room
emptyRoom = MkRoom ((0, 0), DUp) empty empty 0

step :: (Coord, Direction) -> (Coord, Direction)
step ((x, y), DUp) = ((x, y + 1), DUp)
step ((x, y), DDown) = ((x, y - 1), DDown)
step ((x, y), DRight) = ((x + 1, y), DRight)
step ((x, y), DLeft) = ((x - 1, y), DLeft)

advance :: Set Coord -> (Coord, Direction) -> (Coord, Direction)
advance obs (pos, dir) = ans
  where
    forward = step (pos, dir)
    right = step (pos, turn dir)
    back = step (pos, turn2 dir)
    turn2 = turn . turn
    ans
      | fst forward `notMember` obs = forward
      | fst right `notMember` obs = right
      | otherwise = back

inBounds :: Room -> Bool
inBounds MkRoom {sideLength = upper, position = ((x, y), _)} = lowOK && highOK && rightOK && leftOK
  where
    leftOK = x >= 0
    rightOK = x <= upper - 1
    lowOK = y >= 0
    highOK = y <= upper - 1

run :: Room -> Room
run room@MkRoom {obstacles = obs, position = pos, visited = v} =
  if inBounds room
    then continue
    else room
  where
    nextPos = advance obs pos
    v' = insert pos v
    continue = run room {visited = v', position = nextPos}

getPos :: Parsec a Room Coord
getPos = do
  pos <- getPosition
  let x = sourceColumn pos - 2
  let y = sourceLine pos - 1
  return (x, y)

addObstacle :: Coord -> Room -> Room
addObstacle pos room@MkRoom {obstacles = obs} = room {obstacles = insert pos obs}

parseObstacle :: Parsec String Room ()
parseObstacle = char '#' *> getPos >>= modifyState . addObstacle

addPlayer :: Coord -> Room -> Room
addPlayer pos room = room {position = (pos, DUp)}

parsePlayer :: Parsec String Room ()
parsePlayer = char '^' *> getPos >>= modifyState . addPlayer

parseRoomStateful :: Parsec String Room ()
parseRoomStateful = void $ many1 (choice [try parseObstacle, try parsePlayer, void parseChar])

parseRoom :: Parsec String Room Room
parseRoom = parseRoomStateful >> getState

d6e1 :: Solution
d6e1 (filename, contents) = ans
  where
    side = length . lines $ contents
    contents' = intercalate "\n" . reverse . lines $ contents
    initialRoom =
      safeParse
        parseRoom
        emptyRoom {sideLength = side}
        filename
        contents'
    finalRoom = run initialRoom
    visitedCoords = Data.Set.map fst (visited finalRoom)
    ans = show . size $ visitedCoords

generateVariations :: Coord -> Room -> [Set Coord]
generateVariations startingPos MkRoom {obstacles = o, visited = v} =
  nub
    [ insert pos o
      | pos <- positions,
        pos /= startingPos
    ]
  where
    positions = toList $ Data.Set.map fst v

isLoopy :: Room -> Bool
isLoopy room@MkRoom {obstacles = obs, position = pos, visited = v}
  | not $ inBounds room = False
  | isLoop = True
  | otherwise = continue
  where
    nextPos = advance obs pos
    isLoop = pos `member` v
    v' = insert pos v
    continue = isLoopy room {visited = v', position = nextPos}

d6e2 :: Solution
d6e2 (filename, contents) = ans
  where
    side = length . lines $ contents
    contents' = intercalate "\n" . reverse . lines $ contents
    initialRoom =
      safeParse
        parseRoom
        emptyRoom {sideLength = side}
        filename
        contents'
    finalRoom = run initialRoom
    roomVariants =
      [ initialRoom {obstacles = o}
        | o <- generateVariations (fst . position $ initialRoom) finalRoom
      ]
    loopyRooms = filter isLoopy roomVariants
    ans = show . length $ loopyRooms