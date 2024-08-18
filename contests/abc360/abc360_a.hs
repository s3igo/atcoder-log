main :: IO ()
main = do
  [a, b, c] <- getLine
  putStrLn $ if a == 'R' || b == 'R' && c == 'M' then "Yes" else "No"
