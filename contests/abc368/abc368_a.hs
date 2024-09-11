import Data.Tuple

main :: IO ()
main = do
  [n, k] <- readInts
  putStrLn . unwords . map show . uncurry (++) . swap . splitAt (n - k) =<< readInts
  where
    readInts = map (read @Int) . words <$> getLine
