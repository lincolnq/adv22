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

type S = ((Int, Int), [(Int, Int)])

type Visited = S.Set (Int, Int)

moveHead :: Char -> (Int, Int) -> (Int, Int)
moveHead 'R' (x,y) = (x+1, y)
moveHead 'L' (x,y) = (x-1, y)
moveHead 'U' (x,y) = (x, y-1)
moveHead 'D' (x,y) = (x, y+1)



moveFollower :: ((Int,Int),(Int,Int)) -> (Int,Int)
-- no move in y if close
moveFollower ((x,y),d@(tx,ty)) | x==tx, abs(y-ty) < 2 = d
-- no move in x if close
moveFollower ((x,y),d@(tx,ty)) | y==ty, abs(x-tx) < 2 = d

-- move in y if further
moveFollower (h@(x,y),(tx,ty)) | x==tx, y>ty = (tx,ty+1)
moveFollower (h@(x,y),(tx,ty)) | x==tx, y<ty = (tx,ty-1)

-- move in x if further
moveFollower (h@(x,y),(tx,ty)) | y==ty, x>tx = (tx+1,ty)
moveFollower (h@(x,y),(tx,ty)) | y==ty, x<tx = (tx-1,ty)

-- close diagonals
moveFollower ((x,y),d@(tx,ty)) | abs(y-ty)<2, abs(x-tx) < 2 = d

-- farther diagonals
moveFollower (h@(x,y),(tx,ty)) | y>ty, x>tx = (tx+1,ty+1)
moveFollower (h@(x,y),(tx,ty)) | y<ty, x>tx = (tx+1,ty-1)
moveFollower (h@(x,y),(tx,ty)) | y>ty, x<tx = (tx-1,ty+1)
moveFollower (h@(x,y),(tx,ty)) | y<ty, x<tx = (tx-1,ty-1)

moveFollower (_,d) = d

step :: S -> (Char, Int) -> State Visited S
step s0 (c, 0) = return s0
step s0 (c, n) = do
    s1 <- step1 c s0
    step s1 (c, n-1) 

step1 c (h, ts) = do
    
    let nh = moveHead c h

    (_, ts') <- foldM (\(ref, ts') f -> do
        let f' = moveFollower (ref,f)
        return $ (f',ts' ++ [f'])
        ) (nh,[]) ts
--    let f = moveFollower (nh, t)
    
    modify $ S.insert (last ts')
    return $ trace (show (nh, ts')) (nh, ts')

part1 = length . (\p -> execState (foldM_ step ((0,0),[(0,0)]) p) S.empty) . parsed
part2 = length . (\p -> execState (foldM_ step ((0,0),replicate 9 (0,0)) p) S.empty) . parsed