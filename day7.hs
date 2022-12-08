{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isAlpha, isDigit)
import Data.List.Split (splitOn)
import Data.Map (Map ())
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Debug.Trace (trace, traceShow)

data Line
  = CD String
  | Ls
  | DirLine String
  | FileLine Int String
  deriving (Show, Eq)

parseLine :: [Char] -> Line
parseLine ('$' : ' ' : 'c' : 'd' : ' ' : cmd) = CD cmd
parseLine ('$' : ' ' : 'l' : 's' : _) = Ls
parseLine ('d' : 'i' : 'r' : ' ' : dir) = DirLine dir
parseLine line =
  let (size : name : _) = splitOn [' '] line
   in FileLine (read size :: Int) name

fullPath :: [String] -> String -> String
fullPath dirs curdir = foldl (++) curdir dirs

walk2 :: Map String Int -> [String] -> String -> [Line] -> Map String Int
walk2 lookup (parDir : pardirs) curDir [] = Map.adjust (+ Map.findWithDefault 0 (fullPath (parDir : pardirs) curDir) lookup) parDir lookup
walk2 lookup dirs curDir ((CD dir) : ls)
  | dir == ".." =
      let (parDir : pardirs) = dirs
       in walk2 (Map.adjust (+ Map.findWithDefault 0 (fullPath dirs curDir) lookup) (fullPath pardirs parDir) lookup) pardirs parDir ls
  | otherwise =
      walk2 (Map.insert (fullPath (curDir : dirs) dir) 0 lookup) (curDir : dirs) dir ls
walk2 lookup parDir curDir (Ls : ls) = walk2 lookup parDir curDir ls
walk2 lookup parDir curdir ((FileLine size name) : ls) =
  walk2 (Map.adjust (+ size) (fullPath parDir curdir) lookup) parDir curdir ls
walk2 lookup parDir curDir ((DirLine dir) : ls) = walk2 lookup parDir curDir ls

main :: IO ()
main = do
  input <- readFile "input/day7.txt" --  (1783610, 4370655)
  -- input <- readFile "input/sample6.txt" -- 95437,24933642
  let parsedLines = map parseLine $ lines input
      lookup = walk2 (Map.empty :: Map String Int) [] "" parsedLines
      toFree = 30000000 - (70000000 - fromJust (Map.lookup "/" lookup))
      dirToDel = minimum $ filter (> toFree) $ map snd $ Map.toList lookup
  print $ sum $ filter (<= 100000) $ map snd $ Map.toList lookup
  print dirToDel -- 4370655
