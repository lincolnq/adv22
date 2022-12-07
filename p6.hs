import Helpers
import qualified Data.List as L
import qualified Data.Set as S

sample = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"

inp = readFile "input6.txt"
go fn = inp <&> fn
test fn = fn sample
main = (,) <$> go part1 <*> go part2

-- solve: we need to 'tail' the list n times to produce the lists starting from the beginning,
-- 2nd char, 3rd, etc.
-- this is the zipWith.
-- L.Transpose makes a list from the head of each list, then the head of those tails, and so on,
-- so we have a list of 4- or 14-character strings, on which we can do the allDiff trick.
-- We just need to add countUnique at the end since index 0 is the first 4 chars.
ixMessageStart countUnique =
     (+countUnique) . flip ixOf True . map allDiff . 
     L.transpose . zipWith (fpow tail) [0..(countUnique - 1)] . repeat

part1 = ixMessageStart 4
part2 = ixMessageStart 14

