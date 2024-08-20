import Data.List
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)

main :: IO ()
main = do
  n <- readLn @Int
  [as, bs] <- map concat . chunksOf n . lines <$> getContents
  let (i, j) = (`divMod` n) . fromJust . findIndex id $ zipWith (/=) as bs
  putStrLn . unwords $ map (show . succ) [i, j]
