module Main where

increasing :: (Ord a, Num a) => [a] -> Bool
increasing [] = True
increasing [x] = True
increasing (x:y:xs) | y > x && safeDist x y = increasing (y:xs)
                    | otherwise = False

decreasing :: (Ord a, Num a) => [a] -> Bool
decreasing [] = True
decreasing [x] = True
decreasing (x:y:xs) | y < x && safeDist x y = decreasing (y:xs)
                    | otherwise = False

safeDist :: (Ord a, Num a) => a -> a -> Bool 
safeDist x y = abs (x - y) <= 3

safeCheck :: (Ord a, Num a) => [a] -> Bool
safeCheck (x:y:xs) | x > y && safeDist x y = decreasing (y:xs)
                   | x < y && safeDist x y = increasing (y:xs)
                   | otherwise = False

removeAt :: Int -> [a] -> [a]
removeAt i xs = before ++ drop 1 after where (before, after) = splitAt i xs

dampener :: (Ord a, Num a) => [a] -> Bool
dampener ns = or [safeCheck (removeAt i ns) | i <- [0..length ns]] 

main :: IO ()
main = do
  putStrLn "Advent of Code 2024 - Day 2"

  -- Process input
  input <- readFile "input.txt"
  let input' = [[read w :: Integer | w <- words l] | l <- (lines input)]

  -- Part 1
  let result1 = sum [1 | ns <- input', safeCheck ns]
  putStrLn $ "Part 1: " ++ show result1

  -- Part 2
  let result2 = sum [1 | ns <- input', dampener ns]
  putStrLn $ "Part 2: " ++ show result2

