import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List

readInts :: IO [Int]
readInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getContents

main :: IO ()
main = do
  [a, b, c, x] <- readInts
  print $ length [() | a' <- [0 .. a], b' <- [0 .. b], c' <- [0 .. c], 500 * a' + 100 * b' + 50 * c' == x]
