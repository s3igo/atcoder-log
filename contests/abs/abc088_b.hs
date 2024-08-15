import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List

readInts :: IO [Int]
readInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

uninterleave :: [Int] -> ([Int], [Int])
uninterleave = unzip . pairUp
  where
    pairUp [] = []
    pairUp [x] = [(x, 0)]
    pairUp (x : y : xs) = (x, y) : pairUp xs

main :: IO ()
main = do
  as <- getLine >> readInts
  let (a, b) = uninterleave . reverse $ sort as
  print $ sum a - sum b
