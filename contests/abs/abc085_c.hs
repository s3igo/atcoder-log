import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List

readInts :: IO [Int]
readInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main :: IO ()
main = do
  [n, y] <- readInts
  let result = [[a, b, c] | a <- [0 .. n], b <- [0 .. n - a], let c = n - a - b, 10000 * a + 5000 * b + 1000 * c == y]
  putStrLn $ case result of
    [] -> "-1 -1 -1"
    x : _ -> unwords $ map show x
