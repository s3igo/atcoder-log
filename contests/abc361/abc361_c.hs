import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List

readInts :: IO [Int]
readInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main :: IO ()
main = do
  [n, k] <- readInts
  as <- listArray @UArray (0, n - 1) . sort <$> readInts
  print . minimum $ [as ! (i + n - k - 1) - as ! i | i <- [0 .. k]]
