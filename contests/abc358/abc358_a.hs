main :: IO ()
main = do
  [s, t] <- words <$> getLine
  putStrLn $ if s == "AtCoder" && t == "Land" then "Yes" else "No"
