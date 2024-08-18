import Data.Bool (bool)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List

main :: IO ()
main = do
  [xa, ya, xb, yb, xc, yc] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getContents
  let ab2 = (xa - xb) ^ (2 :: Int) + (ya - yb) ^ (2 :: Int)
  let bc2 = (xb - xc) ^ (2 :: Int) + (yb - yc) ^ (2 :: Int)
  let ca2 = (xc - xa) ^ (2 :: Int) + (yc - ya) ^ (2 :: Int)
  putStrLn . bool "No" "Yes" $ ab2 + bc2 == ca2 || bc2 + ca2 == ab2 || ca2 + ab2 == bc2
