main :: IO ()
main = do
  n <- readLn @Int
  putStrLn . take (n * 2 + 1) $ cycle "10"
