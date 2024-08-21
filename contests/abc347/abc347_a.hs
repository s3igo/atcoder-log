main :: IO ()
main = do
  [_, k] <- readInts
  putStrLn . unwords . map (show . (`div` k)) . filter (\a -> a `mod` k == 0) =<< readInts
  where
    readInts = map (read @Int) . words <$> getLine
