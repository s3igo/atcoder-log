main :: IO ()
main = do
  a <- readLn @Int
  [b, c] <- map (read @Int) . words <$> getLine
  s <- getLine
  putStrLn $ unwords [show (a + b + c), s]
