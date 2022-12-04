import Parsers
import Data.List.Split
testInp = readFile "test4.txt"
inp = readFile "input4.txt"
go fn = inp >>= return . fn
test fn = testInp >>= return . fn

parseRanges = map (tup2 . map (tup2 . nums . splitOn "-") . splitOn ",") . lines

fullyIncludes (a,b) (c,d) = a<=c && b>=d
disjoint (a,b) (c,d) = b < c || a > d


p1 = countTrue . map (\e1e2 -> uncurry fullyIncludes e1e2 || uncurry (flip fullyIncludes) e1e2) . parseRanges
p2 = countTrue . map (not . uncurry disjoint) . parseRanges
