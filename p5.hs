import Helpers
import qualified Data.Array.IArray as Array
import qualified Data.Map as M
import Text.Scanf
import Data.Function
import Control.Monad
import Control.Monad.State.Lazy

testInp = readFile "test5.txt"
inp = readFile "input5.txt"
go fn = inp <&> fn
test fn = testInp <&> fn

type Stax = M.Map Int String

data Move = Move { _count :: Int, _src :: Int, _dst :: Int }
    deriving (Show)

parseMove :: String -> Move
parseMove s = Move a b c where
    Just (a :+ b :+ c :+ ()) = scanf (fmt_ ("move " % int . " from " % int . " to " % int) ) s

readStax ix = gets (M.! ix)
putStax ix = modify . M.insert ix
modifyStax ix f = modify (M.adjust f ix)

-- make move ignoring count
doMove1 :: Move -> State Stax ()
doMove1 mv = do
    ~(item:src') <- readStax (_src mv)
    modifyStax (_src mv) tail
    modifyStax (_dst mv) (item:)

-- make a move (repeating the stack motion count times)
doMultiMove :: Move -> State Stax ()
doMultiMove mv = replicateM_ (_count mv) (doMove1 mv)

-- make move ignoring count
doStackMove :: Move -> State Stax ()
doStackMove mv = do
    let c = _count mv
    src <- readStax (_src mv)
    modifyStax (_src mv) (drop c)
    modifyStax (_dst mv) ((take c src)++)

parsed = first (M.fromList . zip [1::Int ..] . map (dropWhile (== ' ') . init) . chunkRows . transpose . Array.amap (!! 1) . readMat 4)
    . second (map parseMove . lines)
    . tup2 . sections

part1 = map head . M.elems . (uncurry $ flip execState) . second (mapM_ doMultiMove) . parsed
part2 = map head . M.elems . (uncurry $ flip execState) . second (mapM_ doStackMove) . parsed

