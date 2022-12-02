module Parsers where

import Text.Read
import Data.List.Split
import Data.Maybe

-- 'lines' does the obvious thing from prelude
-- most stuff starts with 'lines'

-- must read numbers
nums :: [String] -> [Integer]
nums = map read

-- maybe read numbers
maybeNums :: [String] -> [Maybe Integer]
maybeNums = map readMaybe

-- groups of non-nothings separated by nothings
splitMaybes :: Eq a => [Maybe a] -> [[a]]
splitMaybes = map (map fromJust) . splitOn [Nothing]

