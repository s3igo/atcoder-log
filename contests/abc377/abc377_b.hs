import Control.Monad (replicateM)
import Data.List

cnt :: [[Char]] -> Int
cnt = length . filter (all (== '.'))

main :: IO ()
main = do
  ss <- replicateM 8 getLine
  print $ cnt ss * cnt (transpose ss)
