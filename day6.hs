module Day6 where

main :: IO ()
main = do
  contents <- readFile "day6-input"
  print . part1 $ contents

data Fish = Old Int | New Int
  deriving (Eq, Show)

spawn :: [Fish] -> Int -> [Fish]
spawn init 0    = init
spawn init days = spawn (concatMap f init) (days - 1)
  where
    f (Old i) = case next of
                6 -> [Old next, New 8]
                _ -> [Old next]
      where next = mod (i - 1) 7
    f (New i) = if i > 5 then [New (i - 1)] else [Old (i - 1)]

totalFish :: [Fish] -> Int -> Int
totalFish fish days = length $ spawn fish days

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

part1 :: String -> Int
part1 = flip totalFish 80 . map (Old . read) . split ","
