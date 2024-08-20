import Data.List

main :: IO ()
main = do
  [_, k] <- readInts
  let step (cnt, rest) a = if a <= rest then (cnt, rest - a) else (cnt + 1, k - a)
  print . fst . foldl' step (1 :: Int, k) =<< readInts
  where
    readInts = map (read @Int) . words <$> getLine
