import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- readLn @Int
  xys <- replicateM n $ do [x, y] <- map (read @Int) . words <$> getLine; return (x, y)
  putStrLn $ case compare (foldl' (\acc (x, y) -> acc + x - y) 0 xys) 0 of
    GT -> "Takahashi"
    EQ -> "Draw"
    LT -> "Aoki"
