import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Algorithms.Search (binarySearchP)
import Data.Vector.Unboxed qualified as VU

main :: IO ()
main = do
  [n, x, y] <- unfoldr trimInt <$> BS.getLine
  [a, b] <- mapM proc [x, y]
  print $ if a == n && b == n then n else min (a + 1) (b + 1)
  where
    trimInt = BS.readInt . BS.dropWhile isSpace
    proc lim =
      BS.getLine
        >>= VU.thaw . VU.scanl' (+) 0 . VU.modify (VAI.sortBy . flip $ compare) . VU.unfoldr trimInt
        >>= binarySearchP (> lim)
