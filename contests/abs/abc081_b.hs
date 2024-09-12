solve :: [Int] -> Int
solve xs
  | all even xs = 1 + solve (map (`div` 2) xs)
  | otherwise = 0

main :: IO ()
main = print . solve . map (read @Int) . words =<< (getLine >> getLine)
