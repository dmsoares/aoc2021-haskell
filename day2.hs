module Day2 where

import           Control.Monad                     ()
import           System.Directory.Internal.Prelude (getArgs)
import           System.IO

main = do
  content <- readFile "day2-input"
  let linesOfFiles = lines content
  print (part1 linesOfFiles)

part1 :: [String] -> Int
part1 =  uncurry (*) . foldl nextPos (0, 0)

nextPos :: (Int, Int) -> String -> (Int, Int)
nextPos (x, y) str =
  case words str of
    ["forward", n] -> (x + read n, y)
    ["down", n]    -> (x, y + read n)
    ["up", n]      -> (x, y - read n)
