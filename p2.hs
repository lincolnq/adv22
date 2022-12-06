import Helpers

testInp = readFile "test2.txt"
inp = readFile "input2.txt"

go fn = inp <&> fn
test fn = testInp <&> fn

-- we represent RPS shapes 0-1-2 for R-P-S
-- We'll use this shortcut a few places, but basically when we want to find out
-- who's a winner or what strategic choice to make to win/lose/draw, we will be
-- wanting to sum some stuff and then take the mod 3 in order to 'wrap' it
-- to the 0-1-2 range.
mod3 = (`mod` 3)

-- This is the score for a given shape you chose (1=Rock, 2=Paper, 3=Scissors)
shapeScore = succ

-- This is the score for a given (them, us) outcome (either 0, 3, or 6 for W/L/T)
-- Subtract us minus them, and modulo 3, to get the game result, then index into 
-- the scoring array.
winScore (them, us) = [3,6,0] !! mod3 (us-them)

-- Given their choice (RPS shape) and our 'strategy index' from the parser of 0-2,
-- convert it into our RPS shape choice.
-- Study this a while to see why it works.
strategy (them, si) = mod3 (them + si - 1)

-- Parsing the thing - for each line, split it on words, then form a tuple with the char.
-- The first char needs to be indexed into ABC and the second into XYZ to get them both
-- onto 0-2.
parsed = map (first (ixOf "ABC") . second (ixOf "XYZ") . tup2 . map head . words) . lines

-- to compute total score for a series of rps games, we compute winScore and roundShapeScore
-- separately and then add it all up.
totalScore = sum . map (uncurry (+) <<< winScore &&& shapeScore . snd)

-- part1 just returns the total score directly
part1 = totalScore . parsed

-- For part2, we use 'strategy' to replace the snd of the parse with our shape, then do the
-- same as in part1.
part2 = totalScore . map (fst &&& strategy) . parsed
