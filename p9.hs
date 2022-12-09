import Helpers hiding (transpose)
import qualified Data.Set as S
import qualified Data.Vector as V
import Control.Monad.State
import Control.Monad
import Control.Lens
import Debug.Trace
import Data.List

testInp = readFile "test9.txt"
inp = readFile "input9.txt"
go fn = inp <&> fn
test fn = testInp <&> fn

parsed = map (first head . second readInt . tup2 . words) . lines

-- Moving the head just means decoding the direction.
moveHead :: Char -> (Int, Int) -> (Int, Int)
moveHead 'R' (x,y) = (x+1, y)
moveHead 'L' (x,y) = (x-1, y)
moveHead 'U' (x,y) = (x, y-1)
moveHead 'D' (x,y) = (x, y+1)

-- Moving a follower, given the link it's following:
moveFollower :: (Int,Int) -> (Int,Int) -> (Int,Int)
-- if both x and y are within 1, no move.
moveFollower (x,y) d@(tx,ty) | abs (y-ty) <= 1, abs (x-tx) <= 1 = d

-- Otherwise, we move according to the signum of the axis deltas.
-- This ends up handling all remaining cases perfectly: if axis aligned 
-- with our lead then move in the horizontal/vertical direction;
-- otherwise move diagonally.
moveFollower (x,y) (tx,ty) = (tx + signum (x-tx), ty + signum (y-ty))

-- a high level simulator step is just a repeated step1
step s0 (direction, n) = foldl step1 s0 (replicate n direction)

-- step the head in the given direction, returning new state.
step1 (h, ks, visited) direction = (h', ks', S.insert lastKnot visited)
    where
    -- first move the head
    h' = moveHead direction h
    -- then move the knots. note: our accumulator is the same as the 
    -- result of the mapping function, so 'dupe' works nicely
    (lastKnot, ks') = mapAccumL (dupe .: moveFollower) h' ks

-- initial state of the simulation has the head starting at (0,0) 
-- (or anywhere really), as well as the correct number of knots,
-- and no visited locations for the last knot yet.
initialState knots = ((0,0), replicate (knots - 1) (0,0), S.empty)

part1 = length . (^. _3) . foldl step (initialState  2) . parsed
part2 = length . (^. _3) . foldl step (initialState 10) . parsed

main = (,) <$> go part1 <*> go part2