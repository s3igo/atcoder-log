import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List

readInts :: IO [Int]
readInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main :: IO ()
main = do
  as <- getLine >> readInts
  let sorted = sortDesc $ zip as [1 ..]
  print . snd $ sorted !! 1
  where
    sortDesc = sortBy . flip $ compare @(Int, Int)
