main :: IO ()
main = do
  [_, m] <- readInts
  print . length . filter (<= m) . scanl1 (+) =<< readInts
  where
    readInts = map (read @Int) . words <$> getLine
