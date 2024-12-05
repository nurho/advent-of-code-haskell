module Main where
import Text.Regex.TDFA
import Text.Regex

mulRegex = "mul\\([0-9]+,[0-9]+\\)"
dontRegex = "don't\\(\\).*do\\(\\)"

getNums :: String -> (Int, Int)
getNums s = (read (matches !! 0), read (matches !! 1))
  where
    matches = getAllTextMatches (s =~ "[0-9]+") :: [String]

-- Function to remove all matches of a regex pattern from a string
-- removePattern :: String -> String -> String
-- removePattern s pattern = unwords (s =~ pattern :: [String])


main :: IO ()
main = do
  -- Process input
  input <- readFile "input.txt"
  let input' = concat (lines input)

  -- Part 1
  let muls = getAllTextMatches (input' =~ mulRegex) :: [String]
  let pairs = [getNums x | x <- muls]
  let result1 = sum [x * y | (x,y) <- pairs]
  putStrLn $ "Part 1: " ++ show result1

  -- Part 2
  -- let remDont = input' =~ "don't\\(\\).+do\\(\\)" :: String
  let remDont = concat (getAllTextMatches (input' =~ dontRegex) :: [String])
  print remDont
  let muls2 = getTextMatches (remDont =~ mulRegex) :: [String]
  let pairs2 = [getNums x | x <- muls2]
  let result2 = sum [x * y | (x,y) <- pairs2]
  putStrLn $ "Part 2: " ++ show (result1 - result2)
