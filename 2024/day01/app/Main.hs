module Main where
import Data.List

--  ## Advent of Code 2024 ##
--  Day 1:
--  Split each column into a list, sort them, zip, sum the difference of corresponding elements

main :: IO ()
main = do
  putStrLn "Advent of Code 2024 - Day 1"

  -- Process input
  input <- readFile "input.txt"
  let input' = [[read w :: Integer | w <- words l] | l <- (lines input)]

  -- Part 1
  let column1 = sort [l !! 0 | l <- input']
  let column2 = sort [l !! 1 | l <- input']
  let result1 = sum (zipWith (\x y -> abs (x - y)) column1 column2)
  putStrLn $ "Part 1: " ++ show result1

  -- Part 2
  let counts = [(x, count x column2) | x <- column1] where count n xs = sum [1 | x <- xs, n == x]
  let result2 = sum [x * y | (x,y) <- counts]
  putStrLn $ "Part 2: " ++ show result2
