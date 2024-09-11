import Control.Monad
import Data.List

main :: IO ()
main = do
  as <- map (read @Int) . words <$> (getLine >> getLine)
  let Left ans = foldM step as [0 :: Int ..]
  print ans
  where
    step acc i = do
      let (x : y : ys) = sortBy (flip compare) acc
      if x == 0 || y == 0
        then Left i
        else Right $ (x - 1) : (y - 1) : ys
