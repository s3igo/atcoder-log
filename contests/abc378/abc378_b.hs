import Control.Monad (forM_, replicateM)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List

readInts :: IO [Int]
readInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main :: IO ()
main = do
  n <- readLn @Int
  qrs <- replicateM n $ do [q, r] <- map (read @Int) . words <$> getLine; return (q, r)
  q <- readLn @Int
  tds <- replicateM q $ do [t, d] <- map (read @Int) . words <$> getLine; return (t, d)
  forM_ tds $ \(t, d) -> do
    return ()
