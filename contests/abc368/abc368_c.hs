import Data.ByteString.Char8 qualified as BS
import Data.Char
import Data.List

step :: Int -> Int -> Int
step t h
  | h >= 5 = step (t + d * 3) m
  | h > 0 = step (t + 1) (h - dmg)
  | otherwise = t
  where
    (d, m) = h `divMod` 5
    dmg = if (t + 1) `mod` 3 == 0 then 3 else 1

main :: IO ()
main = print . foldl' step 0 . unfoldr (BS.readInt . BS.dropWhile isSpace) =<< (getLine >> BS.getLine)
