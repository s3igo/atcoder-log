import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List
import Data.List.Extra (nubOrd)

readInts :: IO [Int]
readInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getContents

main :: IO ()
main = getLine >> readInts >>= print . length . nubOrd
