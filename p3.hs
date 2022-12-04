import qualified Data.Set as S
import Data.List.Split

testInp = readFile "test3.txt"
inp = readFile "input3.txt"
go fn = inp >>= return . fn
test fn = testInp >>= return . fn

sacks s = (S.fromList $ take n s, S.fromList $ drop n s)
    where n = length s `div` 2

val c | c >= 'a' = fromEnum c - fromEnum 'a' + 1
val c = fromEnum c - fromEnum 'A' + 27

p1 = sum . map (val . head . S.toList . uncurry S.intersection . sacks) . lines
p2 = sum . map (val . head . S.toList . foldl1 S.intersection) . chunksOf 3 . map S.fromList . lines