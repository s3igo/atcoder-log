main :: IO ()
main = do
  [w, b] <- map (read @Int) . words <$> getLine
  let cands = [take (w + b) . drop i $ cycle s' | i <- [0 .. length s' - 1]]
  let check str = length (filter (== 'b') str) == b && length (filter (== 'w') str) == w
  putStrLn $ if any check cands then "Yes" else "No"
  where
    s' = "wbwbwwbwbwbw"
