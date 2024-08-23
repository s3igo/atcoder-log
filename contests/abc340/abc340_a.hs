main :: IO ()
main = do
  [a, b, d] <- map (read @Int) . words <$> getLine
  putStrLn . unwords $ map show [a, a + d .. b]
