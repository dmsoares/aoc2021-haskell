module Day5 where

main :: IO ()
main = do
  contents <- readFile "day5-input"
  print . part1 . pointsOfLines . onlyOrto . parseCoords . lines $ input
  print . part1 . pointsOfLines . onlyOrto . parseCoords . lines $ contents

input = "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2\n"
input' = "0,9 -> 5,9\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n0,9 -> 2,9\n3,4 -> 1,4\n"

split :: String -> String -> [String]
split sep s = go sep s [[]]
  where
    go _ [] acc = reverse acc
    go [] s _ = [s]
    go sep s@(c:s') acc =
      if startsWith sep s
      then go sep (drop (length sep) s) ("":acc)
      else go sep s' ((head acc ++ [c]) : tail acc)
    startsWith s = (== s) . take (length s)

strToPair :: String -> (Int, Int)
strToPair s = (read a, read b)
  where
    r = split "," s
    a = head r
    b = head . tail $ r

parseCoords :: [String] -> [((Int, Int), (Int, Int))]
parseCoords = map ((\[a, b] -> (a, b)) . map strToPair . split " -> ")

alignCoords :: [((Int, Int), (Int, Int))] -> [((Int, Int), (Int, Int))]
alignCoords = map (\((a, b), (c, d)) -> ((a, c), (b, d)))

orderPair :: (Int, Int) -> (Int, Int)
orderPair (x, y) =
  if x >= y
  then (y, x)
  else (x, y)

orderPoints :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
orderPoints (a, b) = (orderPair a, orderPair b)

pointsOfLines :: [((Int, Int), (Int, Int))] -> [(Int, Int)]
pointsOfLines = concatMap (f . orderPoints) . alignCoords
  where
    f ((x, y), (w, z)) = [(a, b) | a <- [x..y], b <- [w..z]]

onlyOrto :: [((Int, Int), (Int, Int))] -> [((Int, Int), (Int, Int))]
onlyOrto = filter (\((a, b), (c, d)) -> a == c || b == d)

part1 :: [(Int, Int)] -> Int
part1 ps = go ps 0 []
  where
    go [] n _ = n
    go [a] n _ = n
    go (x:xs) n r
       | x `elem` xs && x `notElem` r = go xs (n + 1) (x:r)
       | otherwise = go xs n r
