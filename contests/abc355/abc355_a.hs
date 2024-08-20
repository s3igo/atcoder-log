main :: IO ()
main = do
  [a, b] <- map (read @Int) . words <$> getLine
  print $ if a /= b then 6 - a - b else -1
