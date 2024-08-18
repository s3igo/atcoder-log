import Control.Monad (replicateM)
import Data.Bool (bool)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List

cyclicSlice :: Int -> [a] -> [[a]]
cyclicSlice n xs = take (length xs) $ map (take n) $ iterate (\ys -> tail ys ++ [head ys]) xs

main :: IO ()
main = do
  putStrLn . bool "No" "Yes" . any (\[a, b, c] -> a + b == c) . cyclicSlice 3 . map (uncurry dist2 . listToTuple2) . cyclicSlice 2 =<< readCoords
  where
    listToTuple2 [x, y] = (x, y)
    readCoords = replicateM 3 $ listToTuple2 . unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    dist2 (x1, y1) (x2, y2) = (x1 - x2) ^ (2 :: Int) + (y1 - y2) ^ (2 :: Int)
