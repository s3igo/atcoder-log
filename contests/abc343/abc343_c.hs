import Data.List
import Data.Maybe

main :: IO ()
main = do
  n <- readLn @Int
  putStrLn . fromJust . find (\s -> s == reverse s) . map show . reverse $ takeWhile (<= n) [i * i * i | i <- [1 ..]]
