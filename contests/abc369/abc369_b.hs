import Control.Monad (replicateM)
import Data.List
import Data.Tuple.Extra (both)

main :: IO ()
main = do
  n <- readLn @Int
  (ls, rs) <- both (map fst) . partition ((== 'L') . snd) <$> replicateM n (do [a, s] <- words <$> getLine; return (read @Int a, head s))
  print $ calc ls + calc rs
  where
    calc l = sum . map abs . zipWith (-) l $ tail l
