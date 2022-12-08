import Helpers hiding (transpose)
import Data.Traversable
import qualified Data.Matrix as Mat
import qualified Data.Vector as V

testInp = readFile "test8.txt"
inp = readFile "input8.txt"
go fn = inp <&> fn
test fn = testInp <&> fn

grid = fmap readInt . readMat 1

-- for each row in matrix, convert that row to a Vector, run the function on that Vector, then 
-- accumulate and reassemble into a Matrix
mapRows f = Mat.fromRows . map f . Mat.toRows
mirror = mapRows V.reverse

runMirror     f = mirror . f . mirror
runTransposed f = Mat.tr . f . Mat.tr

-- [Part 1] unblocked viewing in this direction: 1 if unblocked, 0 if blocked by anything
isUnblockedRow = snd . mapAccumL (\s it -> (max s it, fromEnum (it > s))) (negate 1)

-- [Part 2] count of viewing distance in this direction: 0 if on edge, otherwise number of squares 
-- to reach next higher than self
viewingDistance = snd . mapAccumL (\s it -> (it:s, length . takeUntil (>=it) $ s)) []

-- Take f (some kind of function on the grid) and m (the grid).
-- Combine mirror and transpose to run f in each direction; returns the list of results
-- thus produced.
doFromAllDirections f m = map (\x -> x f m) [id, runMirror, runTransposed, runTransposed . runMirror]

part1 = countTrue . fmap (> 0) . foldl1 (Mat.zipWith (+)) . doFromAllDirections (mapRows isUnblockedRow)  . grid
part2 = maximum .                foldl1 (Mat.zipWith (*)) . doFromAllDirections (mapRows viewingDistance) . grid