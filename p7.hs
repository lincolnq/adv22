import Helpers
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

testInp = readFile "test7.txt"
inp = readFile "input7.txt"
go fn = inp <&> fn
test fn = testInp <&> fn

--main = (,) <$> go part1 <*> go part2
main = go part1

data Traverse = Traverse { _cwd :: [String], _files :: M.Map [String] Int, _dirs :: S.Set [String]}
    deriving (Show)

emptyFS = Traverse [] M.empty (S.singleton [])

trav :: Traverse -> String -> Traverse
trav t "$ cd /"  
    = t { _cwd = [] }
trav t@Traverse{_cwd} "$ cd .."  
    = t { _cwd = tail _cwd }
trav t@Traverse{_cwd} s | "$ cd " `isPrefixOf` s 
    = t { _cwd = (drop 5 s) : _cwd }

-- ignore ls lines
trav t s | "$ ls" `isPrefixOf` s = t

-- ignore dirs in ls, they don't contribute to file sizes
trav t@Traverse{_cwd, _dirs} s | "dir " `isPrefixOf` s 
    = t { _dirs= S.insert (reverse _cwd ++ [drop 4 s]) _dirs }
trav t@Traverse{_cwd, _files} s = t { _files= M.insert (reverse _cwd ++ [filename]) size _files }
    where (size, filename) = first read . tup2 . words $ s

filesystem = foldl trav emptyFS . lines

fsquery :: ([String]->Bool) -> Traverse -> [([String], Int)]
fsquery ff fs = filter (ff . fst) (M.toList . _files $ fs)

dirsize :: Traverse -> [String] -> Int
dirsize fs dirname = sum . map snd . fsquery (dirname `isPrefixOf`) $ fs

part1 = sum . filter (<=100000) . (\fs -> map (dirsize fs) (S.toList $ _dirs fs)) . filesystem

part2 = (\fs -> 
    let dirSizes = map (\d -> (d, dirsize fs d)) (S.toList $ _dirs fs) in
    let spaceAtRoot = (M.fromList dirSizes) M.! [] in 
    let currentFree = 70000000 - spaceAtRoot in 
    let mustFree = 30000000 - currentFree in 
    minimum . filter (>=mustFree) . map snd $ dirSizes
    ) . filesystem