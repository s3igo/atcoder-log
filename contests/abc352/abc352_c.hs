import Control.Monad (replicateM)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List

readInts :: IO [Int]
readInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main :: IO ()
main = do
  [n] <- readInts
  [as, bs] <- transpose <$> replicateM n readInts
  print $ sum as + maximum (zipWith (-) bs as)
