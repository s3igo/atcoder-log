import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List

readInts :: IO [Int]
readInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main :: IO ()
main = do
  [n, t, p] <- readInts
  ls <- listArray @UArray (1, n) . sortBy (flip compare) <$> readInts
  print . max 0 $ t - (ls ! p)
