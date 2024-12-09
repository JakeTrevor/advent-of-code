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

d9e2 :: Solution
d9e2 (_, contents) = ans
  where
    nums = [read [c] | c <- contents]
    expanded = expandDiskMap 0 nums
    fixed = reverse . compactify 0 $ reverse expanded
    total = checkSum $ catMaybes fixed
    ans = show total