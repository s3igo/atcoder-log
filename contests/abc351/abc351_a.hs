main :: IO ()
main = do
  a <- getSum
  b <- getSum
  print $ a - b + 1
  where
    getSum = sum . map (read @Int) . words <$> getLine
