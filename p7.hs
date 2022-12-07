{-# LANGUAGE TemplateHaskell #-}

import Helpers
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Lens

testInp = readFile "test7.txt"
inp = readFile "input7.txt"
go fn = inp <&> fn
test fn = testInp <&> fn

data Traverse = Traverse { _cwd :: [String], _files :: M.Map [String] Int, _dirs :: S.Set [String]}
    deriving (Show)

makeLenses ''Traverse

emptyFS = Traverse [] M.empty (S.singleton [])

-- cwd is stored with the directory stack on top, whereas files and dirs are stored
-- directory stack reversed (so they sort according to the fileystem tree). 
-- This function is used to convert between, for storage.
appendPath cwd n = reverse cwd ++ [n]

trav :: Traverse -> String -> Traverse

-- handle cd traversals:
trav t "$ cd /"
    = t & cwd .~ []
trav t "$ cd .."  
    = t & cwd %~ tail
trav t s | Just dirname <- stripPrefix "$ cd " s
    = t & cwd %~ (dirname:)

-- ignore ls lines
trav t s | "$ ls" `isPrefixOf` s = t

-- handle dirs and files
trav t@Traverse{_cwd} s | Just dirname <- stripPrefix "dir " s
    = t & dirs %~ (S.insert $ _cwd `appendPath` dirname)
trav t@Traverse{_cwd} s | (size, filename) <- first read . tup2 . words $ s
    = t & files %~ (M.insert (_cwd `appendPath` filename) size)

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
-- (yes this is a bit much... sorry :/)
part2 = minimum . uncurry filter <<< (<=) . (+size_offset) . rootsize &&& dirSizes <<< readFilesystem

main = (,) <$> go part1 <*> go part2
