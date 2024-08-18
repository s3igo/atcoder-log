import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List

readInts :: IO [Int]
readInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

binarySearch :: (Integral a) => a -> a -> (a -> Bool) -> a
binarySearch ng ok p
  | abs (ok - ng) <= 1 = ok
  | p mid = binarySearch ng mid p
  | otherwise = binarySearch mid ok p
  where
    mid = (ok + ng) `div` 2

calc :: Int -> [Int] -> Int
calc lim = foldl' (\acc x -> acc + min lim x) 0

main :: IO ()
main = do
  [_, m] <- readInts
  as <- readInts
  let idx = binarySearch 1 m (\x -> calc x as > m)
  putStrLn $ if idx /= m then show $ idx - 1 else "infinite"
