import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List
import Data.Vector.Unboxed qualified as VU

readInts :: IO [Int]
readInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main :: IO ()
main = do
  [n] <- readInts
  as <- map pred <$> readInts
  ws <- readInts
  print $ sum ws - VU.sum (VU.accumulate max (VU.replicate n 0) . VU.fromList $ zip as ws)
