module Day3 where

import           Data.List (transpose)

main :: IO ()
main = do
  content <- readFile "day3-input"
  let linesOfFile = lines content
  print . part1 $ linesOfFile

gammaPartial :: String -> Bool
gammaPartial str =
  n >= length str `div` 2
  where
    n :: Int
    n = foldr (\a b -> read [a] + b) 0 str

epsilonPartial :: String -> Bool
epsilonPartial str =
  n < length str `div` 2
  where
    n :: Int
    n = foldr (\a b -> read [a] + b) 0 str

binToDec :: [Bool] -> Int
binToDec = fst . foldr f (0, 0)
  where f True (n, p) = (n + 2 ^ p, p + 1)
        f _ (n, p)    = (n, p + 1)

part1 :: [String] -> Int
part1 lines =
  let transposed = transpose lines
  in
    (binToDec . map gammaPartial $ transposed) *
    (binToDec . map epsilonPartial $ transposed)
