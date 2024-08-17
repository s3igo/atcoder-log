import Control.Monad (foldM)
import Data.Bool (bool)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List
import Data.Maybe (isJust)
import Data.Sequence (replicateM)

readInts :: IO [Int]
readInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

listToTuple3 :: [a] -> (a, a, a)
listToTuple3 [x, y, z] = (x, y, z)
listToTuple3 _ = undefined

main :: IO ()
main = do
  [n] <- readInts
  txys <- replicateM n $ listToTuple3 <$> readInts
  putStrLn . bool "No" "Yes" . isJust $ foldM aux (0, 0, 0) txys
  where
    aux (t0, x0, y0) (t, x, y) =
      if d <= dt && even (dt - d) then Just (t, x, y) else Nothing
      where
        dt = t - t0
        dx = abs (x - x0)
        dy = abs (y - y0)
        d = dx + dy
