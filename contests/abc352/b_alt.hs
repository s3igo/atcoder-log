solve :: String -> String -> [Int]
solve = go 1
  where
    go _ [] _ = []
    go _ _ [] = []
    go acc (x : xs) (y : ys)
      | x == y = acc : go (acc + 1) xs ys
      | otherwise = go (acc + 1) (x : xs) ys

main :: IO ()
main = do
  s <- getLine
  t <- getLine
  putStrLn . unwords . map show $ solve s t
