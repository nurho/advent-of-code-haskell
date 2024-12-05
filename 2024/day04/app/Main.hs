module Main where

getV :: (Eq a, Eq b) => a -> b -> [(a, b, c)] -> Maybe c
getV x y tuples =
    case filter (\(a, b, _) -> a == x && b == y) tuples of
        [(_, _, z)] -> Just z
        _           -> Nothing

-- Part 1 Find functions
-- Cardinals
findFwd x y g = if (getV (x+1) y g) == (Just 'M') &&
                   (getV (x+2) y g) == (Just 'A') &&
                   (getV (x+3) y g) == (Just 'S')
                then 1
                else 0

findBwd x y g = if (getV (x-1) y g) == (Just 'M') &&
                   (getV (x-2) y g) == (Just 'A') &&
                   (getV (x-3) y g) == (Just 'S')
                then 1
                else 0

findUp x y g = if (getV x (y-1) g) == (Just 'M') &&
                  (getV x (y-2) g) == (Just 'A') &&
                  (getV x (y-3) g) == (Just 'S')
               then 1
               else 0

findDown x y g = if (getV x (y+1) g) == (Just 'M') &&
                    (getV x (y+2) g) == (Just 'A') &&
                    (getV x (y+3) g) == (Just 'S')
                 then 1
                 else 0

-- Diagonals
findDRU x y g = if (getV (x+1) (y-1) g) == (Just 'M') &&
                   (getV (x+2) (y-2) g) == (Just 'A') &&
                   (getV (x+3) (y-3) g) == (Just 'S')
                then 1
                else 0

findDRD x y g = if (getV (x+1) (y+1) g) == (Just 'M') &&
                   (getV (x+2) (y+2) g) == (Just 'A') &&
                   (getV (x+3) (y+3) g) == (Just 'S')
                then 1
                else 0
findDLU x y g = if (getV (x-1) (y-1) g) == (Just 'M') &&
                   (getV (x-2) (y-2) g) == (Just 'A') &&
                   (getV (x-3) (y-3) g) == (Just 'S')
                then 1
                else 0
findDLD x y g = if (getV (x-1) (y+1) g) == (Just 'M') &&
                   (getV (x-2) (y+2) g) == (Just 'A') &&
                   (getV (x-3) (y+3) g) == (Just 'S')
                then 1
                else 0

-- Part 2 Find functions
-- M . M
-- . A .
-- S . S
findMas1 x y g = if (getV (x-1) (y-1) g) == (Just 'M') &&
                    (getV (x+1) (y-1) g) == (Just 'M') &&
                    (getV (x-1) (y+1) g) == (Just 'S') &&
                    (getV (x+1) (y+1) g) == (Just 'S')
                 then 1
                 else 0
                                        
-- S . M
-- . A .
-- S . M
findMas2 x y g = if (getV (x-1) (y-1) g) == (Just 'S') &&
                    (getV (x+1) (y-1) g) == (Just 'M') &&
                    (getV (x-1) (y+1) g) == (Just 'S') &&
                    (getV (x+1) (y+1) g) == (Just 'M')
                 then 1
                 else 0

-- S . S
-- . A .
-- M . M
findMas3 x y g = if (getV (x-1) (y-1) g) == (Just 'S') &&
                    (getV (x+1) (y-1) g) == (Just 'S') &&
                    (getV (x-1) (y+1) g) == (Just 'M') &&
                    (getV (x+1) (y+1) g) == (Just 'M')
                 then 1
                 else 0

-- M . S
-- . A .
-- M . S
findMas4 x y g = if (getV (x-1) (y-1) g) == (Just 'M') &&
                    (getV (x+1) (y-1) g) == (Just 'S') &&
                    (getV (x-1) (y+1) g) == (Just 'M') &&
                    (getV (x+1) (y+1) g) == (Just 'S')
                 then 1
                 else 0

main :: IO ()
main = do

  -- Process input
  input <- readFile "input.txt"
  let input' = lines input

  let w = length (head input')
  let l = length input'
  let grid = [(x,y,(input' !! y) !! x) | x <- [0..(w-1)], y <- [0..(l-1)]] 

  -- Part 1

  let fwds = sum [findFwd x y grid | (x,y,z) <- grid, z == 'X']
  let bwds = sum [findBwd x y grid | (x,y,z) <- grid, z == 'X']
  let ups = sum [findUp x y grid | (x,y,z) <- grid, z == 'X']
  let downs = sum [findDown x y grid | (x,y,z) <- grid, z == 'X']
  let drus = sum [findDRU x y grid | (x,y,z) <- grid, z == 'X']
  let drds = sum [findDRD x y grid | (x,y,z) <- grid, z == 'X']
  let dlus = sum [findDLU x y grid | (x,y,z) <- grid, z == 'X']
  let dlds = sum [findDLD x y grid | (x,y,z) <- grid, z == 'X']
  let result1 = fwds + bwds + ups + downs + drus + drds + dlus + dlds
  putStrLn $ "Part 1: " ++ show result1

  -- Part 2
  let mas1s = sum [findMas1 x y grid | (x,y,z) <- grid, z == 'A']
  let mas2s = sum [findMas2 x y grid | (x,y,z) <- grid, z == 'A']
  let mas3s = sum [findMas3 x y grid | (x,y,z) <- grid, z == 'A']
  let mas4s = sum [findMas4 x y grid | (x,y,z) <- grid, z == 'A']
  let result2 = mas1s + mas2s + mas3s + mas4s
  putStrLn $ "Part 2: " ++ show result2
