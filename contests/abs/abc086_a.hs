main :: IO ()
main = do
  [a, b] <- map (read @Int) . words <$> getLine
  putStrLn $ if even $ a * b then "Even" else "Odd"
