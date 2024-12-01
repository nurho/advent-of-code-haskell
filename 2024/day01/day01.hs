-- Advent of Code 2024
-- Day 1

-- Split each column into a list, sort them, zip, sum the difference of corresponding elements
import System.IO  
import Control.Monad

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = (qsort lesser) ++ [x] ++ (qsort greater)
    where
        lesser  = filter (< x) xs
        greater = filter (>= x) xs

count n xs = sum [1 | x <- xs, n == x]

main = do
  input <- readFile "input.txt"

  -- part 1
  let split_input = [words l | l <- (lines input)]
  let list1 = qsort [read (head w) :: Integer | w <- split_input]
  let list2 = qsort [read (head (reverse w)) :: Integer | w <- split_input]
  let pairs = zip list1 list2
  let result1 = sum [abs (x - y) | (x,y) <- pairs]
  print result1

  -- part 2
  let counts = [(x, count x list2) | (x,_) <- pairs]
  let result2 = sum [x * y | (x,y) <- counts]
  print result2

  
