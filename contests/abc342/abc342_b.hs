import Control.Monad
import Data.Array.IArray
import Data.Array.Unboxed (UArray)

main :: IO ()
main = do
  n <- readLn @Int
  ps <- array @UArray (1, n) . flip zip [1 :: Int ..] <$> readInts
  q <- readLn @Int
  replicateM_ q $ do
    [a, b] <- readInts
    print $ if ps ! a < ps ! b then a else b
  where
    readInts = map (read @Int) . words <$> getLine
