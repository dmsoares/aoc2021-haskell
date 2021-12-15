module Day4 where

import           Data.List (transpose)

main :: IO ()
main = do
  contents <- readFile "day4-input"
  let numbers = parseNumbers . head $ lines contents
      boards = flip parseBoards 5 . drop 1 . filter (/= "") $ lines contents
  print $ part1 boards numbers

part1 boards numbers = score $ game (map buildBoard boards) numbers

-- copied this utility from https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

parseNumbers :: String -> [Int]
parseNumbers = map read . wordsWhen (== ',')

parseBoards :: [String] -> Int -> [[String]]
parseBoards [] _ = []
parseBoards lines boardHeight = foldr f [[]] lines
  where
    f a (b:bs) =
      if length b < boardHeight
      then (a:b):bs
      else [a]:b:bs

type Line = [Maybe Int]
type Board = [Line]

buildBoard :: [String] -> Board
buildBoard = map (map intToMaybe . words)
  where
    intToMaybe n = Just (read n)

markBoard :: Int -> Board -> Board
markBoard n = map (map marker)
  where
    marker (Just e) = if e == n then Nothing else Just e
    marker Nothing  = Nothing

isLineComplete :: Line -> Bool
isLineComplete = all (== Nothing)

isBoardComplete :: Board -> Bool
isBoardComplete b = foldr f False b || foldr f False (transpose b)
  where f a b = isLineComplete a || b

playRound :: Int -> [Board] -> [Board]
playRound n = map (markBoard n)

findWinner :: [Board] -> [Board]
findWinner = filter isBoardComplete

sumMaybeInt :: [Maybe Int] -> Int
sumMaybeInt = foldr f 0
  where
    f Nothing b  = b
    f (Just a) b = a + b

game :: [Board] -> [Int] -> (Maybe Board, Maybe Int)
game bs ns = go bs ns Nothing
  where go bs (n:ns) lastN
          | not (null winner) = (Just (head winner), lastN)
          | otherwise = go (playRound n bs) ns (Just n)
          where winner = findWinner bs
        go _ _ _ = (Nothing, Nothing)

score :: (Maybe Board, Maybe Int) -> Int
score (Just b, Just n) = n * (sum . map sumMaybeInt $ b)
score _                = 0

-- samples

sample = map buildBoard [b1, b3]

b1 = [ "22 13 17 11  0"
     , " 8  2 23  4 24"
     , "21  9 14 16  7"
     , " 6 10  3 18  5"
     , " 1 12 20 15 19" ]

b2 = [ " 3 15  0  2 22"
     , " 9 18 13 17  5"
     , "19  8  7 25 23"
     , "20 11 10 24  4"
     , "14 21 16 12  6" ]

b3 = [ "14 21 17 24  4"
     , "10 16 15  9 19"
     , "18  8 23 26 20"
     , "22 11 13  6  5"
     , " 2  0 12  3  7" ]

ns :: [Int]
ns = [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]
