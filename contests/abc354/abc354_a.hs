main :: IO ()
main = do
  h <- readLn @Int
  print . length . takeWhile (<= h) $ map (pred . (2 ^)) ([0 ..] :: [Int])
