import Control.Monad (replicateM)
import Data.List

main :: IO ()
main = do
  n <- readLn @Int
  as <- replicateM n $ map (read @Int) . words <$> getLine
  let step acc i = as !! (max acc i - 1) !! (min acc i - 1)
  print $ foldl' step 1 [1 .. n]
