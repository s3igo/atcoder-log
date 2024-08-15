import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List

readInts :: IO [Int]
readInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main :: IO ()
main = do
  [a] <- readInts
  [b, c] <- readInts
  s <- BS.getLine
  putStrLn $ unwords [show (a + b + c), BS.unpack s]
