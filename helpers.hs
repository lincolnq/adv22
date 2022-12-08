module Helpers (
    module Helpers, 
    module Control.Arrow,
    module Data.Functor,
    module Data.Function,
    module Data.Maybe,
    module Data.List.Split,
    sort,
) where

import Text.Read
import Data.Traversable
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Matrix as Mat
import qualified Data.Matrix.Generic as MG
import qualified Data.Vector as V

import Control.Arrow
import Data.Functor
import Data.Function
import qualified Data.Set as S


-- 'lines' does the obvious thing from prelude
-- most stuff starts with 'lines'
readInt :: String -> Int
readInt = read

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
-- (deprecated: prefer 'first' and 'second')
ff f1 f2 = first f1 . second f2

-- repeat f n times to its input
fpow :: (c -> c) -> Int -> c -> c
fpow f n = foldl (.) id (replicate n f)

-- True if all elements of the list are different
allDiff :: Ord a => [a] -> Bool
allDiff l = length l == (S.size $ S.fromList l)

-- find index of 'c' in 'l'
ixOf :: Eq a => [a] -> a -> Int
ixOf l c = fromJust (elemIndex c l)

-- how many Trues
countTrue :: Traversable t => t Bool -> Int
countTrue = sum . fmap fromEnum

-- Take all elements until one matches. The matching element is returned too.
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p = foldr (\x ys -> x : if p x then [] else ys) []

-- like `lines` but for double blank lines
sections :: String -> [String]
sections = split (dropDelims . dropBlanks $ onSublist "\n\n")


fliptup = uncurry (flip (,))


-- convert a String into a Matrix. The 'rows' (1st index) are separated by blank lines.
-- The 'columns' (2nd index) are fixed width, given by the 'c' parameter.
-- We use 0-based indexing. 
readMat :: Int -> String -> Mat.Matrix String
readMat c inp = Mat.fromLists . map (chunksOf c) . lines $ inp

-- | Display a matrix as a 'String' using the 'Show' instance of its elements.
prettyMatrix :: Show a => Mat.Matrix a -> String
prettyMatrix m = concat
   [ "┌ ", unwords (replicate (Mat.cols m) blank), " ┐\n"
   , unlines
   [ "│ " ++ unwords (fmap (\j -> fill $ strings Mat.! (i,j)) [0..Mat.cols m - 1]) ++ " │" | i <- [0..Mat.rows m - 1] ]
   , "└ ", unwords (replicate (Mat.cols m) blank), " ┘"
   ]
 where
   strings@(MG.Matrix _ _ _ _ v)  = Mat.map show m
   widest = V.maximum $ fmap length v
   fill str = replicate (widest - length str) ' ' ++ str
   blank = fill ""


instance Functor Mat.Matrix where
    fmap f = Mat.map f

instance Foldable Mat.Matrix where
    foldMap f (MG.Matrix _ _ _ _ v) = foldMap f v

instance Traversable Mat.Matrix where
    traverse f = sequenceA . fmap f