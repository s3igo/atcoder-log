import Data.List
import Data.Tuple.Extra (both)

main :: IO ()
main = do
  (a, b) <- both (sum . map snd) . partition (odd . fst) . zip [1 :: Int ..] . sortBy (flip compare) . map (read @Int) . words <$> (getLine >> getLine)
  print $ a - b
