import Parsers
import Data.List

p1 = head . reverse . sort . map sum . splitMaybes . maybeNums . lines
p2 = sum . take 3 . reverse . sort . map sum . splitMaybes . maybeNums . lines 

inp = readFile "input1.txt"

-- not a parser, but useful
try fn = inp >>= return . fn
