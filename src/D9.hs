module D9 (d9) where

import Data.Maybe (catMaybes)
import Lib (DaySolution, Solution)

d9 :: DaySolution
d9 = (d9e1, d9e2)

expandDiskMap :: Int -> [Int] -> [Maybe Int]
expandDiskMap _ [] = []
expandDiskMap n [x] = replicate x $ Just n
expandDiskMap n (x : y : rest) = xs ++ dots ++ expandDiskMap (n + 1) rest
  where
    xs = expandDiskMap n [x]
    dots = replicate y Nothing

replace :: Maybe Int -> [Maybe Int] -> [Maybe Int]
replace c (Nothing : rest) = c : rest
replace c (x : rest) = x : replace c rest
replace _ [] = error "Cannot replace into an empty string"

compactify :: Int -> [Maybe Int] -> [Maybe Int]
compactify n [] = replicate n Nothing
compactify n (Nothing : str) = compactify (n + 1) str
compactify n str@(x : rest)
  | Nothing `notElem` str = str
  | otherwise = compactify (n + 1) replacedStr
  where
    replacedStr = reverse . replace x . reverse $ rest

checkSum :: [Int] -> Int
checkSum str = foldr ((+) . uncurry (*)) 0 strIndex
  where
    strIndex = zip str [0 ..]

d9e1 :: Solution
d9e1 (_, contents) = ans
  where
    nums = [read [c] | c <- contents]
    expanded = expandDiskMap 0 nums
    fixed = reverse . compactify 0 $ reverse expanded
    total = checkSum $ catMaybes fixed
    ans = show total

-- part 2 is hard

data FSObject = File {fSize :: Int, fId :: Int, moved :: Bool} | FEmpty Int

instance Show FSObject where
  show File {fSize = sz, fId = id} = concat $ replicate sz $ show id
  show (FEmpty sz) = concat . replicate sz $ "."

convertList :: Int -> [Int] -> [FSObject]
convertList _ [] = []
convertList n [x] = [File x n False]
convertList n (x : y : rest) = newFile : FEmpty y : convertList (n + 1) rest
  where
    newFile = File x n False

normalise :: [FSObject] -> [FSObject]
normalise [] = []
normalise (f@File {moved = True} : rest) = f : normalise rest
normalise (f@(FEmpty _) : rest) = f : normalise rest
normalise (f : rest) = normalise $ move f {moved = True} rest

move :: FSObject -> [FSObject] -> [FSObject]
move f@File {fSize = sz} fs = ans
  where
    (lst, didMove) = shift f $ reverse fs
    lst' = reverse lst
    modified = FEmpty sz : lst'
    ans = if didMove then modified else lst'
move _ _ = error "cannot call on anything else"

shift :: FSObject -> [FSObject] -> ([FSObject], Bool)
shift f [] = ([f], False)
shift f@File {fSize = fsz} (e@(FEmpty esz) : rest)
  | esz == fsz = (f : rest, True)
  | esz > fsz = (f : FEmpty (esz - fsz) : rest, True)
  | otherwise = let (lst, didMove) = shift f rest in (e : lst, didMove)
shift f (e : rest) = let (lst, didMove) = shift f rest in (e : lst, didMove)

expandFSObject :: FSObject -> [Int]
expandFSObject (FEmpty sz) = replicate sz 0
expandFSObject (File {fSize = sz, fId = fid}) = replicate sz fid

d9e2 :: Solution
d9e2 (_, contents) = ans
  where
    nums = [read [c] | c <- contents]
    fList = convertList 0 nums
    fixed = reverse . normalise . reverse $ fList
    total = checkSum $ concatMap expandFSObject fixed
    ans = show total