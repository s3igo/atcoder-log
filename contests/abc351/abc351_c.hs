import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List

readInts :: IO [Int]
readInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main :: IO ()
main = do
  print . length . foldr step [] . reverse =<< (getLine >> readInts)
  where
    step a [] = [a]
    step a acc@(x : xs)
      | a /= x = a : acc
      | otherwise = step (x + 1) xs
