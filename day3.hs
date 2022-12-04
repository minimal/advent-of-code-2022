{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import qualified Data.Set as Set
import Debug.Trace

input =
  "vJrwpWtwJgWrhcsFMMfFFhFp\n\
  \jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\n\
  \PmmdzqPrVvPwwTWBwg\n\
  \wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\n\
  \ttgJtRGJQctTZtZT\n\
  \CrZsJsPPZsGzwwsLwLmpwMDw"

main :: IO ()
main = do
  input <- readFile "input/day3.txt"
  let prios = map winner $ lines input
  print $ sum prios
  where
    winner line =
      let (a, b) = splitAt (length line `div` 2) line
          both = Set.elemAt 0 $ Set.intersection (Set.fromList a) (Set.fromList b)
       in if isLower both then fromEnum both - 96 else fromEnum both - 38
