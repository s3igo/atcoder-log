import Control.Monad (replicateM)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.IntMap.Strict qualified as IM
import Data.List

readInts :: IO [Int]
readInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

accumIntMap :: (a -> a -> a) -> a -> [(Int, a)] -> IM.IntMap a
accumIntMap f initial = foldl' step IM.empty
  where
    step m (k, v) = IM.insertWith f k (f initial v) m

main :: IO ()
main = do
  [n] <- readInts
  cas <- replicateM n $ do [a, c] <- readInts; return (c, a)
  print . maximum . map snd . IM.toList $ accumIntMap min maxBound cas
