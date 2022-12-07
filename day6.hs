{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isAlpha, isDigit)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  input <- readFile "input/day6.txt"
  print $ process input
  where
    process = checkStream 0 ""
    checkStream :: Int -> String -> String -> Int
    checkStream idx buf [] = idx
    checkStream idx buf (x : stream)
      | length buf == 14 = idx
      | x `notElem` buf = checkStream (idx + 1) (buf ++ [x]) stream
      | otherwise = checkStream (idx + 1) ((splitOn [x] buf !! 1) ++ [x]) stream
