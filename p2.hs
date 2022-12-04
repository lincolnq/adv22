import Parsers
import Data.List
import Control.Arrow

testInp = readFile "test2.txt"
inp = readFile "input2.txt"

go fn = inp >>= return . fn
test fn = testInp >>= return . fn

mod3 = (`mod` 3)

shapeScore rpi = rpi + 1
shapeScore2 (_, rpi) = rpi + 1

winScore (them, us) = [3,6,0] !! mod3 (us-them)

strategy (them, si) = mod3 (them + si - 1)

--parseRPS = map (ff  . tup2 . map head . words) . lines

parseRow = arr (arr words ^>> map head ^>> tup2 >>> (ixOf "ABC") *** (ixOf "XYZ"))
parseRPS = map parseRow . lines



p1a = first winScore &&& second shapeScore2 >>> arr (uncurry (+))
p1 = sum . map p1a . parseRPS

--p1 = sum . map (\(them, us) -> shapeScore us + winScore them us) . parseRPS
--p2 = sum . map (\(them, si) -> 
    --let us = mod3 (them + si - 1)
    --in shapeScore us + winScore them us
    --) . parseRPS