import Parsers
import Data.List.Split
import qualified Data.Array.IArray as Array
import Text.Scanf
import qualified Data.Map as M
import Control.Arrow
import Data.Function


testInp = readFile "test5.txt"
inp = readFile "input5.txt"
go fn = inp >>= return . fn
test fn = testInp >>= return . fn

type State = M.Map Int String

data Move = Move { _count :: Int, _src :: Int, _dst :: Int }
    deriving (Show)

parseMove :: String -> Move
parseMove s = Move a b c where
    Just (a :+ b :+ c :+ ()) = scanf (fmt_ ("move " % int . " from " % int . " to " % int) ) s

-- make move ignoring count
doMove1 :: State -> Move -> State
doMove1 st mv = M.insert (_dst mv) dst' $
                M.insert (_src mv) src' $
                st 
    where
    (item:src') = st M.! _src mv
    dst = st M.! _dst mv
    dst' = item:dst

-- make a move (repeating the stack motion count times)
doMultiMove :: State -> Move -> State
doMultiMove st mv = foldl doMove1 st $ replicate (_count mv) mv

-- make move ignoring count
doStackMove :: State -> Move -> State
doStackMove st mv = st 
    & M.insert (_dst mv) dst'
    & M.insert (_src mv) src'
    where
    c = _count mv
    src = st M.! _src mv
    src' = drop c src
    dst = st M.! _dst mv
    dst' = take c src ++ dst


parsed = first (M.fromList . zip [1..] . map (dropWhile (== ' ') . init) . chunkRows . transpose . Array.amap (!! 1) . readMat 4)
    . second (map parseMove . lines)
    . tup2 . sections

part1 = map head . M.elems . uncurry (foldl doMultiMove) . parsed
part2 = map head . M.elems . uncurry (foldl doStackMove) . parsed
