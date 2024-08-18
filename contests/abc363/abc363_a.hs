main :: IO ()
main = do
  r <- readLn @Int
  print $ 100 - r `mod` 100
