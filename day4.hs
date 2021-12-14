module Day4 where

import           Data.List (transpose)

main :: IO ()
main = do
  contents <- readFile "day4-input"
  let linesOfFile = lines contents
  print linesOfFile

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

game :: [Board] -> [Int] -> Maybe Int -> (Maybe Board, Maybe Int)
game bs (n:ns) lastN
  | not (null winner) = (Just (head winner), lastN)
  | otherwise = game (playRound n bs) ns (Just n)
  where winner = findWinner bs
game _ _ _ = (Nothing, Nothing)

score :: (Maybe Board, Maybe Int) -> Int
score (Just b, Just n) = n * (sum . map sumMaybeInt $ b)
score _                = 0

-- samples

b1 = [ "22 13 17 11  0"
     , " 8  2 23  4 24"
     , "21  9 14 16  7"
     , " 6 10  3 18  5"
     , " 1 12 20 15 19" ]

b3 = [ "14 21 17 24  4"
     , "10 16 15  9 19"
     , "18  8 23 26 20"
     , "22 11 13  6  5"
     , " 2  0 12  3  7" ]

ns :: [Int]
ns = [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]
