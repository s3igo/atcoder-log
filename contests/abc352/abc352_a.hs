contains :: (Int, Int) -> Int -> Bool
(l, u) `contains` n = l <= n && n <= u

main :: IO ()
main = do
  [_, x, y, z] <- map (read @Int) . words <$> getLine
  putStrLn $ if (min x y, max x y) `contains` z then "Yes" else "No"
