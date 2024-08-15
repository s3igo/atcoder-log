import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List

readInts :: IO [Int]
readInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

contains :: (Int, Int) -> Int -> Bool
(l, r) `contains` n = l <= n && n <= r

digitsSum :: Int -> Int
digitsSum 0 = 0
digitsSum n = n `mod` 10 + digitsSum (n `div` 10)

main :: IO ()
main = do
  [n, a, b] <- readInts
  print $ sum [i | i <- [1 .. n], (a, b) `contains` digitsSum i]
