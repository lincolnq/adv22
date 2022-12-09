import Helpers hiding (transpose)
import qualified Data.Set as S
import qualified Data.Vector as V
import Control.Monad.State
import Control.Monad
import Debug.Trace

testInp = readFile "test9.txt"
inp = readFile "input9.txt"
go fn = inp <&> fn
test fn = testInp <&> fn

parsed = map (first head . second readInt . tup2 . words) . lines

type S = ((Int, Int), (Int, Int))

type Visited = S.Set (Int, Int)

moveHead :: Char -> S -> S
moveHead 'R' ((x,y), t) = ((x+1, y),t)
moveHead 'L' ((x,y), t) = ((x-1, y),t)
moveHead 'U' ((x,y), t) = ((x, y-1),t)
moveHead 'D' ((x,y), t) = ((x, y+1),t)



moveTail :: S -> S
-- no move in y if close
moveTail d@((x,y),(tx,ty)) | x==tx, abs(y-ty) < 2 = d
-- no move in x if close
moveTail d@((x,y),(tx,ty)) | y==ty, abs(x-tx) < 2 = d

-- move in y if further
moveTail (h@(x,y),(tx,ty)) | x==tx, y>ty = (h, (tx,ty+1))
moveTail (h@(x,y),(tx,ty)) | x==tx, y<ty = (h, (tx,ty-1))

-- move in x if further
moveTail (h@(x,y),(tx,ty)) | y==ty, x>tx = (h, (tx+1,ty))
moveTail (h@(x,y),(tx,ty)) | y==ty, x<tx = (h, (tx-1,ty))

-- close diagonals
moveTail d@((x,y),(tx,ty)) | abs(y-ty)<2, abs(x-tx) < 2 = d

-- farther diagonals
moveTail (h@(x,y),(tx,ty)) | y>ty, x>tx = (h, (tx+1,ty+1))
moveTail (h@(x,y),(tx,ty)) | y<ty, x>tx = (h, (tx+1,ty-1))
moveTail (h@(x,y),(tx,ty)) | y>ty, x<tx = (h, (tx-1,ty+1))
moveTail (h@(x,y),(tx,ty)) | y<ty, x<tx = (h, (tx-1,ty-1))

moveTail d = d

step :: S -> (Char, Int) -> State Visited S
step s0 (c, 0) = return s0
step s0 (c, n) = do
    s1 <- step1 c s0
    step s1 (c, n-1) 

step1 c s0 = do
    
    let s1 = moveHead c s0
    let s2 = moveTail s1
    
    modify $ S.insert (snd s2)
    return $ trace (show s2) s2

part1 = length . (\p -> execState (foldM_ step ((0,0),(0,0)) p) S.empty) . parsed