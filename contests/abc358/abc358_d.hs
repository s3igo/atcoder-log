import Data.ByteString.Char8 qualified as BS
import Data.Char
import Data.List

readInts :: IO [Int]
readInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main :: IO ()
main = do
  as <- sort <$> (getLine >> readInts)
  print . fst . foldl' step (0, as) . sort =<< readInts
  where
    step (acc, x : xs) b
      | x >= b = (acc + x, xs)
      | otherwise = step (acc, xs) b
    step _ _ = (-1, [])
