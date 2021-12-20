module Day5 where

main :: IO ()
main = do
  contents <- readFile "day5-input"
  print . concatMap pointsFromLine . onlyOrto . parseCoords . lines $ input
  print . length . concatMap pointsFromLine . onlyOrto . parseCoords . lines $ input
  print . part1 . concatMap pointsFromLine . parseCoords . lines $ contents

input = "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2\n"
input' = "0,9 -> 5,9\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n0,9 -> 2,9\n3,4 -> 1,4\n"

type Point = (Int, Int)

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

strToPair :: String -> Point
strToPair s = (read a, read b)
  where
    r = split "," s
    a = head r
    b = head . tail $ r

parseCoords :: [String] -> [(Point, Point)]
parseCoords = map ((\[a, b] -> (a, b)) . map strToPair . split " -> ")

pointsFromLine :: (Point, Point) -> [Point]
pointsFromLine (pa, pb) = go pa pb []
  where
    go a@(xa, ya) b@(xb, yb) acc
      | a == b = a : acc
      | xa == xb = go (xa, ya') b (a : acc)
      | ya == yb = go (xa', ya) b (a : acc)
      | otherwise = go (xa', ya') b (a : acc)
      where
        xa' = if xa > xb then xa - 1 else xa + 1
        ya' = if ya > yb then ya - 1 else ya + 1

onlyOrto :: [(Point, Point)] -> [(Point, Point)]
onlyOrto = filter (\((a, b), (c, d)) -> a == c || b == d)

part1 :: [Point] -> Int
part1 ps = go ps 0 []
  where
    go [] n _ = n
    go [a] n _ = n
    go (x:xs) n r
       | x `elem` xs && x `notElem` r = go xs (n + 1) (x:r)
       | otherwise = go xs n r
