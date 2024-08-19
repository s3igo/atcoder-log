import Control.Monad
import Data.Bool
import Data.List

main :: IO ()
main = do
  [(n, _)] <- reads <$> getLine
  putStrLn . bool "No" "Yes" . all ((<= 0) . foldl1 (-)) . transpose =<< replicateM (n + 1) (map (read @Int) . words <$> getLine)
