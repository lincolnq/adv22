import Helpers
import qualified Data.Array.IArray as Array
import qualified Data.Map as M
import Text.Scanf
import Control.Monad
import Control.Monad.State.Lazy

testInp = readFile "test5.txt"
inp = readFile "input5.txt"
go fn = inp <&> fn
test fn = testInp <&> fn

-- This is the first problem where we define our own types. In this case
-- we want a shorthand for the type representing the current stacks:
type Stax = M.Map Int String

-- And when we parse our Moves, we could just use a 3-tuple, but it seemed
-- a bit clearer to actually create a record:
data Move = Move { _count :: Int, _src :: Int, _dst :: Int }
    deriving (Show)

-- This converts a one-line move definition string into a Move.
-- I decided to use the scanf library, but that may have been a silly
-- decision since I couldn't get its Template Haskell working, so it's
-- a bit verbose.
parseMove :: String -> Move
parseMove s = Move a b c where
    Just (a :+ b :+ c :+ ()) = scanf (fmt_ ("move " % int . " from " % int . " to " % int) ) s

-- We're going to use the State monad storing our Stax state.
-- Write a few shortcuts to make our State monad usage clearer. 
-- This gets the stack at a stack index.
readStax ix = gets (M.! ix)

-- This modifies the stack at the given index using a function.
modifyStax ix f = modify (M.adjust f ix)

-- Ok, simulation function to apply a Move to a Stax state. This moves
-- one block at a time (ignoring the count parameter of the move),
-- in 3 steps -- a read followed by 2 writes.
doMove1 :: Move -> State Stax ()
doMove1 mv = do
    ~(item:src') <- readStax (_src mv)
    modifyStax (_src mv) tail
    modifyStax (_dst mv) (item:)

-- Fully implements a Move, by doing the doMove1 the correct number of times.
doMultiMove :: Move -> State Stax ()
doMultiMove mv = replicateM_ (_count mv) (doMove1 mv)

-- move the entire 'count' stack at once
doStackMove :: Move -> State Stax ()
doStackMove mv = do
    let c = _count mv
    src <- readStax (_src mv)
    modifyStax (_src mv) (drop c)
    modifyStax (_dst mv) ((take c src)++)

-- Parser! 
-- The stacks are annoying to parse, so we are doing a bunch of heavy lifting here.
-- First we need to convert the input into Sections by splitting on the blank line.
-- With the first section (the initial stacks), we need to construct a Stax:
--   First parse it into a Matrix by grabbing 4 chars at a time ("[X] ").
--   But then just map it to grab 2nd char and discard the rest.
--   Transpose the Matrix and convert it into a list, so that each row is 1 stack, 
--    and that row grows towards the start of the list, like a stack.
--   Drop the final matrix entry as well as all the leading empty blocks
--   Name each stack and load it into our Map.
-- With the second section (the moves), just parseMove each one.
parsed = first (
        M.fromList . zip [1::Int ..] 
        . map (dropWhile (== ' ') . init) 
        . chunkRows . transpose 
        . Array.amap (!! 1) . readMat 4
        )
    . second (map parseMove . lines)
    . tup2 . sections

-- Use 'f' to execute a movelist against a starting state, returning the final state.
exec :: (m -> State s ()) -> (s, [m]) -> s
exec f (s0,ms) = execState (mapM_ f ms) s0

-- Part1 and part2 are identical except for the move logic:
part1 = map head . M.elems . exec doMultiMove . parsed
part2 = map head . M.elems . exec doStackMove . parsed

