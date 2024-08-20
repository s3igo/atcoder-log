import Data.List
import Data.Maybe (fromJust)

main :: IO ()
main = do
  n <- readLn @Int
  [ss, cs] <- transpose . map words . lines <$> getContents
  let num = sum (map (read @Int) cs) `mod` n
  putStrLn . fromJust . lookup num . zip [0 ..] $ sort ss
