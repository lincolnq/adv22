import Helpers
import qualified Data.List as L
import qualified Data.Set as S

sample = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"

inp = readFile "input6.txt"
go fn = inp <&> fn
test fn = fn sample

fpow n f = foldl (.) id (replicate n f)
allDiff l = length l == (S.size $ S.fromList l)

ixMessageStart countUnique x = countUnique +
     (flip ixOf True . map allDiff . L.transpose $ map (\n -> fpow n tail x) [0..(countUnique - 1)])

part1 = ixMessageStart 4
part2 = ixMessageStart 14

main = do
    a <- go part1
    b <- go part2
    return (a,b)