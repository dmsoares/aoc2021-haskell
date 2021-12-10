module Day2 where

import           Control.Monad                     ()
import           System.Directory.Internal.Prelude (getArgs)
import           System.IO

main = do
  content <- readFile "day2-input"
  let linesOfFiles = lines content
  print (part1 linesOfFiles)

part1 :: [String] -> Int
part1 = (\(Pos x y p) -> p) . foldl nextPos (Pos 0 0 1)

type PosX = Int
type PosY = Int
type Prod = Int

data Move = Forward Int | Down Int | Up Int
data Position = Pos PosX PosY Prod

toTuple :: String -> Move
toTuple = makeTuple . words
  where
    makeTuple [d, n] =
      case d of
        "forward" -> Forward (read n)
        "down"    -> Down (read n)
        "up"      -> Up (read n)

nextPos :: Position -> String -> Position
nextPos (Pos x y p) str =
  case toTuple str of
    (Forward n) -> Pos (x + n) y ((x + n) * y)
    (Down n)    -> Pos x (y + n) (x * (y + n))
    (Up n)      -> Pos x (y - n) (x * (y - n))

sample = lines "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"
