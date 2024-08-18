import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List

readInts :: IO [Int]
readInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main :: IO ()
main = do
  [_, k] <- readInts
  mapM_ (putStrLn . unwords . map show) . filter (\xs -> sum xs `mod` k == 0) . mapM (\r -> [1 .. r]) =<< readInts
