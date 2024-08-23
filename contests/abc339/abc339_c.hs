import Data.ByteString.Char8 qualified as BS
import Data.Char
import Data.List

readInts :: IO [Int]
readInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main :: IO ()
main = print . foldl' (\acc a -> max 0 $ acc + a) 0 =<< (getLine >> readInts)
