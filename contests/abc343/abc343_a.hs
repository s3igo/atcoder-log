main :: IO ()
main = do
  [a, b] <- map (read @Int) . words <$> getLine
  print $ if a + b == 0 then 1 else 0 :: Int
