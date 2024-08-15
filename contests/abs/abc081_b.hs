import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List

readInts :: IO [Int]
readInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

solve :: [Int] -> Int
solve = aux 0
  where
    aux acc xs
      | all even xs = aux (acc + 1) $ map (`div` 2) xs
      | otherwise = acc

main :: IO ()
main = getLine >> readInts >>= print . solve
