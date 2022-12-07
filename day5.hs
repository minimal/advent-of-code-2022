{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isAlpha, isDigit)
import Data.List.Split (splitOn)

input :: String
input =
  "    [D]\n\
  \[N] [C]\n\
  \[Z] [M] [P]\n\
  \ 1   2   3 \n\
  \\n\
  \move 1 from 2 to 1\n\
  \move 3 from 1 to 3\n\
  \move 2 from 2 to 1\n\
  \move 1 from 1 to 2"

updateList :: Int -> a -> [a] -> [a]
updateList n val lst =
  let (x, _ : xs) = splitAt n lst
   in x ++ val : xs

data Move = Move Int Int Int
  deriving (Ord, Eq, Show, Read)

main :: IO ()
main = do
  input <- readFile "input/day5.txt"
  print $ process $ lines input
  where
    process rows =
      let (stacks' : moves : _) = splitOn [""] rows
          stacks = take (length stacks' - 1) stacks'
          colWidths = map (colWidth . length) stacks
          stackLetters = zipWith getRowCrates colWidths stacks
          nCols = maximum $ map length stackLetters
          transposed = map (filter isAlpha) $ transpose stackLetters nCols
          finalStacks = foldl doMove transposed (map parseMove moves)
          finalStacks2 = foldl doMove2 transposed (map parseMove moves)
       in (map head finalStacks, map head finalStacks2)
    colWidth n = (n + 1) `div` 4
    getRowCrates width row = map (getCrate row) [0 .. width - 1]
    doMove :: [[a]] -> Move -> [[a]]
    doMove stacks (Move n from to) =
      if n == 0
        then stacks
        else
          let newToVal = head (stacks !! from) : (stacks !! to)
              newFromVal = tail (stacks !! from)
           in doMove (updateList from newFromVal $ updateList to newToVal stacks) (Move (n - 1) from to)
    doMove2 stacks (Move n from to) =
      let newToVal = take n (stacks !! from) ++ (stacks !! to)
          newFromVal = drop n (stacks !! from)
       in (updateList from newFromVal $ updateList to newToVal stacks)
    getCrate row n = row !! (((n + 1) * 4) - 3)
    transpose rows ncols = map (bleh rows) [0 .. (ncols - 1)]
    bleh rows idx = foldr (pluck idx) [] rows
    pluck idx r a = if idx < length r then (r !! idx) : a else a
    parseMove move =
      let (n : from : to : _) = map (\c -> read c :: Int) $ filter (/= "") $ map (filter isDigit) $ splitOn [' '] move
       in Move n (from - 1) (to - 1)
