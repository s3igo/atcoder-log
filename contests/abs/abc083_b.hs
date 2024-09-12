import Data.Ix (inRange)

digits :: Int -> [Int]
digits 0 = []
digits n = m : digits d
  where
    (d, m) = n `divMod` 10

main :: IO ()
main = do
  [n, a, b] <- map (read @Int) . words <$> getLine
  print $ sum [i | i <- [1 .. n], inRange (a, b) (sum $ digits i)]
