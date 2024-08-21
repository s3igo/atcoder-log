main :: IO ()
main = do
  l : xs <- getLine
  putStrLn $ if l == '<' && all (== '=') (init xs) && last xs == '>' then "Yes" else "No"
