import Data.Bool (bool)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List

readInts :: IO [Int]
readInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main :: IO ()
main = do
  [n, t, a] <- readInts
  let rest = n - t - a
  putStrLn . bool "No" "Yes" $ (min t a + rest) < max t a
