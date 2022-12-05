module Parsers where

import Text.Read
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Array.IArray as Array
import Control.Arrow


-- 'lines' does the obvious thing from prelude
-- most stuff starts with 'lines'

-- must read numbers
nums :: [String] -> [Int]
nums = map read

-- maybe read numbers
maybeNums :: [String] -> [Maybe Int]
maybeNums = map readMaybe

-- groups of non-nothings separated by nothings
splitMaybes :: Eq a => [Maybe a] -> [[a]]
splitMaybes = map (map fromJust) . splitOn [Nothing]

-- convert a 2-list into a 2-tuple
tup2 = (\[a,b] -> (a,b))

-- apply 2 functions to left and right of 2-tuple
-- (prefer 'first' and 'second')
ff f1 f2 = first f1 . second f2

-- find index of 'c' in 'l'
ixOf :: Eq a => [a] -> a -> Int
ixOf l c = fromJust (elemIndex c l)

-- how many Trues
countTrue :: [Bool] -> Int
countTrue = sum . map fromEnum

-- like `lines` but for double blank lines
sections :: String -> [String]
sections = split (dropDelims . dropBlanks $ onSublist "\n\n")

-- convert a String into a 2d array. The 'rows' (1st index) are separated by blank lines.
-- The 'columns' (2nd index) are fixed width, given by the 'c' parameter.
-- We use 0-based indexing. 
readMat :: Int -> String -> Array.Array (Int, Int) String
readMat c inp = Array.listArray ((0,0), (h-1,w-1)) $ concat l 
    where 
        l :: [[String]] = map (chunksOf c) $ lines inp
        w = length (head l)
        h = length l

-- convert a 2d array (as read by readMat) into a list of lists.
chunkRows :: Array.Array (Int, Int) a -> [[a]]
chunkRows a = map (map snd) $ chunksOf (w+1) $ Array.assocs a where
    (_,(_,w)) = Array.bounds a

-- show matrix by showing all the rows separated by blank lines. (kind of awk)
showMat :: Show a => Array.Array (Int, Int) a -> String
showMat = intercalate "\n" . map show . chunkRows

fliptup = uncurry (flip (,))

-- transpose matrix 2d
transpose :: Array.Array (Int, Int) a -> Array.Array (Int, Int) a
transpose a = Array.ixmap (second fliptup $ Array.bounds a) fliptup a

