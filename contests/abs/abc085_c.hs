main :: IO ()
main = do
  [n, y] <- map (read @Int) . words <$> getLine
  putStrLn $ case [[i, j, k] | i <- [0 .. n], j <- [0 .. n - i], let k = n - i - j, 10000 * i + 5000 * j + 1000 * k == y] of
    [] -> "-1 -1 -1"
    x : _ -> unwords $ map show x
