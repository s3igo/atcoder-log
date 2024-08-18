solve :: Int -> Int
solve x
  | x `mod` 4 /= 0 = 365
  | x `mod` 100 /= 0 = 366
  | x `mod` 400 /= 0 = 365
  | otherwise = 366

main :: IO ()
main = readLn >>= print . solve
