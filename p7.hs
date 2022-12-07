import Helpers
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

testInp = readFile "test7.txt"
inp = readFile "input7.txt"
go fn = inp <&> fn
test fn = testInp <&> fn

main = (,) <$> go part1 <*> go part2

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

trav t@Traverse{_cwd, _dirs} s | "dir " `isPrefixOf` s 
    = t { _dirs= S.insert (reverse _cwd ++ [drop 4 s]) _dirs }
trav t@Traverse{_cwd, _files} s = t { _files= M.insert (reverse _cwd ++ [filename]) size _files }
    where (size, filename) = first read . tup2 . words $ s

-- to read the filesystem, fold 'traverse' on all the lines,
-- starting with an empty filesystem/traversal state
readFilesystem = foldl trav emptyFS . lines

-- Query for the size of a directory, given the traversed filesystem:
--  Get all the files where given 'dirname' is a prefix; then just sum their sizes.
dirsize dirname = sum . map snd . filter (isPrefixOf dirname . fst) . M.toList . _files
rootsize = dirsize []

-- Query for the sizes of all directories of the traversed filesystem:
--  Take the filesystem and get its _dirs; for each one, get its dirsize.
dirSizes = uncurry map <<< flip dirsize &&& S.toList . _dirs

-- Each directory is compared to the size of the root minus 40M (to leave 30M)
-- to determine if it is big enough to bother deleting.
size_offset = 30000000 - 70000000::Int

part1 = sum . filter (<=100000) . dirSizes . readFilesystem

-- We need to get the dirsize of the root (left &&&), create a comparison function for 
-- purposes of the filter, then filter the dirSizes and take the minimum.
part2 = minimum . uncurry filter <<< (<=) . (+size_offset) . rootsize &&& dirSizes <<< readFilesystem
