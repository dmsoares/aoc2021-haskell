module Day1 where

import           Control.Monad
import           System.IO

main = do
  let list = []
  handle <- openFile "day1-input" ReadMode
  contents <- hGetContents handle
  let measurements = map read . words $ contents
  print $ part1 measurements
  print $ part2 measurements
  hClose handle

part1 :: [Int] -> Int
part1 [] = 0
part1 [x] = 0
part1 [x, y] = if y > x then 1 else 0
part1 (x:y:rest) | y > x = 1 + part1 (y:rest)
                 | otherwise = part1 (y:rest)

part2 :: [Int] -> Int
part2 = part1 . map sum . three

three :: [Int] -> [[Int]]
three []           = []
three [x]          = []
three [x, y]       = []
three [x, y, z]    = [[x, y, z]]
three (x:y:z:rest) = [x, y, z] : three (y:z:rest)
