import Control.Monad (replicateM)
import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List
import Data.Maybe (fromJust)

readInts :: IO [Int]
readInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main :: IO ()
main = do
  [h, w] <- readInts
  [si, sj] <- readInts
  css <- listArray @UArray ((1, 1), (h, w)) . concat <$> replicateM h getLine
  let aux pos c = move css pos . fromJust $ lookup c directions
  getLine >>= putStrLn . tupleUnwords . foldl' aux (si, sj)
  where
    directions = [('L', (0, -1)), ('R', (0, 1)), ('U', (-1, 0)), ('D', (1, 0))]
    move mat (i, j) (di, dj) =
      if inRange (bounds mat) (i', j') && mat ! (i', j') == '.' then (i', j') else (i, j)
      where
        (i', j') = (i + di, j + dj)
    tupleUnwords (a, b) = unwords $ map show [a, b]
