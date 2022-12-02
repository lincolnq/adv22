import Parsers
import Data.List

testInp = readFile "test2.txt"
inp = readFile "input2.txt"

go fn = inp >>= return . fn
test fn = testInp >>= return . fn

mod3 = (`mod` 3)

shapeScore rpi = rpi + 1

winScore them us = [3,6,0] !! mod3 (us-them)

parseRPS = map (ff (ixOf "ABC") (ixOf "XYZ") . tup2 . map head) . map words . lines

p1 = sum . map (\(them, us) -> shapeScore us + winScore them us) . parseRPS
p2 = sum . map (\(them, si) -> 
    let us = mod3 (them + si - 1)
    in shapeScore us + winScore them us
    ) . parseRPS