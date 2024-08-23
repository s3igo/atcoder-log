{-# LANGUAGE LambdaCase #-}

import Control.Monad
import Data.IntMap.Strict qualified as IM

main :: IO ()
main = do
  q <- readLn @Int
  foldM_ step (IM.empty, 0 :: Int) [1 .. q]
  where
    readInts = map (read @Int) . words <$> getLine
    step (acc, end) _ = do
      readInts >>= \case
        [1, x] -> return (IM.insert end x acc, end + 1)
        [2, k] -> print (acc IM.! (end - k)) >> return (acc, end)
        _ -> undefined
