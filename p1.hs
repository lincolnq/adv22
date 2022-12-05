import Helpers

p1 = maximum . map sum . splitMaybes . maybeNums . lines
p2 = sum . take 3 . reverse . sort . map sum . splitMaybes . maybeNums . lines 

inp = readFile "input1.txt"

-- not a parser, but useful
try fn = inp <&> fn
