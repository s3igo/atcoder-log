import Control.Monad
import Data.ByteString.Char8 qualified as BS
import Data.Char
import Data.List

readInts :: IO [Int]
readInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main :: IO ()
main = do
  as <- getLine >> readInts
  acc <- foldM step 0 $ init as
  print $ acc + last as
  where
    step acc a = do
      [s, t] <- readInts
      return $ (acc + a) `div` s * t
