--{-# LANGUAGE OverloadedStrings #-}

scoreRound :: String -> Int
scoreRound ['A',_,'X'] = 1 + 3
scoreRound ['A',_,'Y'] = 2 + 6
scoreRound ['A',_,'Z'] = 3 + 0
scoreRound ['B',_,'X'] = 1 + 0
scoreRound ['B',_,'Y'] = 2 + 3
scoreRound ['B',_,'Z'] = 3 + 6
scoreRound ['C',_,'X'] = 1 + 6
scoreRound ['C',_,'Y'] = 2 + 0
scoreRound ['C',_,'Z'] = 3 + 3

scoreRound2 :: String -> Int
scoreRound2 ['A',_,'X'] = 3 + 0
scoreRound2 ['A',_,'Y'] = 1 + 3
scoreRound2 ['A',_,'Z'] = 2 + 6
scoreRound2 ['B',_,'X'] = 1 + 0
scoreRound2 ['B',_,'Y'] = 2 + 3
scoreRound2 ['B',_,'Z'] = 3 + 6
scoreRound2 ['C',_,'X'] = 2 + 0
scoreRound2 ['C',_,'Y'] = 3 + 3
scoreRound2 ['C',_,'Z'] = 1 + 6

main :: IO ()
main = do
  input <- readFile "input/day2.txt"
  let rounds = lines input
      scores = map scoreRound rounds
      scores2 = map scoreRound2 rounds
  print [sum scores,sum scores2]
