import Control.Monad (replicateM)
import Data.Function (on)
import Data.List

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) ^ (2 :: Int) + abs (y1 - y2) ^ (2 :: Int)

maxIdx :: (Int, Int) -> [(Int, Int)] -> Int
maxIdx xy xys = fst . maximumBy (compare `on` snd) . reverse $ zip [1 ..] [distance xy xy' | xy' <- xys]

main :: IO ()
main = do
  n <- readLn @Int
  xys <- replicateM n $ do [x, y] <- map (read @Int) . words <$> getLine; return (x, y)
  mapM_ print [maxIdx xy xys | xy <- xys]
