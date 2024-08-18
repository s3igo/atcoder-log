import Data.Bool (bool)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List

readInts :: IO [Int]
readInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

contains :: (Int, Int) -> Int -> Bool
(l, u) `contains` n = l <= n && n <= u

main :: IO ()
main = do
  [a, b, c] <- readInts
  putStrLn . bool "Yes" "No" $ if b < c then (b, c) `contains` a else not $ (c, b) `contains` a
