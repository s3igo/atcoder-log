main :: IO ()
main = do
  [a, b] <- map (read @Int) . words <$> getLine
  print $ if a == b then 1 :: Int else if even (a + b) then 3 else 2
