import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List
import Data.List.Extra (nubSortBy)

readInts :: IO [Int]
readInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main :: IO ()
main = do
  [_, k] <- readInts
  as <- dropWhile (> k) . nubSortBy (flip compare) <$> readInts
  print $ (1 + k) * k `div` 2 - sum as
