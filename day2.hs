module Day2 where

import           Control.Monad                     ()
import           System.Directory.Internal.Prelude (getArgs)
import           System.IO

main = do
  content <- readFile "day2-input"
  let linesOfFiles = lines content
  print (part1 linesOfFiles)

part1 :: [String] -> Int
part1 = (\(x, y, p) -> p) . foldl nextPos (0, 0, 1)

nextPos :: (Int, Int, Int) -> String -> (Int, Int, Int)
nextPos (x, y, n) str =
  case words str of
    ["forward", n] -> (x + read n, y, (x + read n) * y)
    ["down", n]    -> (x, y + read n, x * y + read n)
    ["up", n]      -> (x, y - read n, x * y - read n)
