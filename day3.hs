module Day3 where

import           Data.List (transpose)

main :: IO ()
main = do
  content <- readFile "day3-input"
  let linesOfFile = lines content
  print $ part1 linesOfFile
  print $ part2 linesOfFile

gammaPartial :: String -> Bool
gammaPartial str =
  n >= (length str `div` 2) + (length str `mod` 2)
  where
    n = foldr (\a b -> read [a] + b) 0 str

gammaToEpsilon :: [Bool] -> [Bool]
gammaToEpsilon = map not

binToDec :: [Bool] -> Int
binToDec = fst . foldr f (0, 0)
  where f True (n, p) = (n + 2 ^ p, p + 1)
        f _ (n, p)    = (n, p + 1)

strToBin :: String -> [Bool]
strToBin = map (== '1')

oxyGen :: [String] -> [String]
oxyGen = go 0
  where
    go _ [] = []
    go _ [l] = [l]
    go i m =
      if gammaPartial . (!! i) . transpose $ m
      then go (i + 1) $ filter ((== '1') . (!! i)) m
      else go (i + 1) $ filter ((== '0') . (!! i)) m

co2Scrub :: [String] -> [String]
co2Scrub = go 0
  where
    go _ [] = []
    go _ [l] = [l]
    go i m =
      if not . gammaPartial . (!! i) . transpose $ m
      then go (i + 1) $ filter ((== '1') . (!! i)) m
      else go (i + 1) $ filter ((== '0') . (!! i)) m

part1 :: [String] -> Int
part1 lines =
  let transposed = transpose lines
      gamma = map gammaPartial transposed
      epsilon = gammaToEpsilon gamma
  in
    binToDec gamma * binToDec epsilon

part2 :: [String] -> Int
part2 lines =
  binToDec (strToBin . head . oxyGen $ lines) *
  binToDec (strToBin . head . co2Scrub $ lines)
