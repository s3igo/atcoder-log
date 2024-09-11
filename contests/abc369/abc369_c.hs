import Data.List

main :: IO ()
main = do
  n <- readLn @Int
  as <- map (read @Int) . words <$> getLine
  print $ n + sum (map (calc . length) . group $ zipWith (-) (tail as) as)
  where
    calc n = n * (n + 1) `div` 2
