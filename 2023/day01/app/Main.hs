module Main where
import Data.Char

--  ## Advent of Code 2023 ##
--  Day 1:
--  Find the first and last digits in each line, form a two-digit number from them and sum them all

findDigit :: [Char] -> Char
findDigit [] = ' '
findDigit (x:xs) = if isDigit x then x else findDigit xs

nums = ["zero","one","two","three","four","five","six","seven","eight","nine"]

findnum s (x:xs) | s == x = 0
                 | otherwise = 1 + findnum s xs

parse :: String -> String -> Int
parse (c:cs) prev | isDigit c = digitToInt c
                  | elem (prev ++ [c]) nums = findnum (prev ++ [c]) nums
                  | elem prev3 nums = findnum prev3 nums 
                  | elem prev4 nums = findnum prev4 nums 
                  | elem prev5 nums = findnum prev5 nums 
                  | otherwise = parse cs (prev ++ [c])
                where
                  prev3 = take 3 (reverse (prev ++ [c]))
                  prev4 = take 4 (reverse (prev ++ [c]))
                  prev5 = take 5 (reverse (prev ++ [c]))

rparse :: String -> String -> Int
rparse (c:cs) prev | isDigit c = digitToInt c
                   | elem (reverse (prev ++ [c])) nums = findnum (reverse (prev ++ [c])) nums
                   | elem (reverse prev3) nums = findnum (reverse prev3) nums 
                   | elem (reverse prev4) nums = findnum (reverse prev4) nums 
                   | elem (reverse prev5) nums = findnum (reverse prev5) nums 
                   | otherwise = rparse cs (prev ++ [c])
                where
                  prev3 = take 3 (reverse (prev ++ [c]))
                  prev4 = take 4 (reverse (prev ++ [c]))
                  prev5 = take 5 (reverse (prev ++ [c]))

main :: IO ()
main = do
  putStrLn "Advent of Code 2023 - Day 1"

  -- Process input
  input <- readFile "input.txt"
  let input' = lines input

  -- Part 1
  let result1 = sum [read ([findDigit l] ++ [findDigit (reverse l)]) :: Integer | l <- input'] 
  putStrLn $ "Part 1: " ++ show result1


  -- Part 2
  let p2 = [(show (parse l [])) ++ (show (rparse (reverse l) [])) | l <- input']
  let result2 = sum [read x :: Integer | x <- p2]
  putStrLn $ "Part 2: " ++ show result2

