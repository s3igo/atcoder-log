main :: IO ()
main = do
  [a, b, c, d, e, f, g, h, i, j, k, l] <- map (read @Int) . words <$> getContents
  let x = isIntersected (a, d) (g, j)
      y = isIntersected (b, e) (h, k)
      z = isIntersected (c, f) (i, l)
  putStrLn $ if x && y && z then "Yes" else "No"
  where
    isIntersected (l1, r1) (l2, r2) = l2 < r1 && l1 < r2
