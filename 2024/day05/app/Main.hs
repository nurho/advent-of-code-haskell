module Main where

main :: IO ()
main = do
  -- Process input
  input <- readFile "input.txt"
  let (rules, updates) = (r, drop 1 t) where (r,t) = break null (lines input)
  print updates

  -- Part 1

