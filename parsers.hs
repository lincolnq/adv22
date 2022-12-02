module Parsers where

import Text.Read
import Data.List
import Data.List.Split
import Data.Maybe


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
ff f1 f2 (a,b) = (f1 a, f2 b)

-- find index of 'c' in 'l'
ixOf :: Eq a => [a] -> a -> Int
ixOf l c = fromJust (elemIndex c l)
