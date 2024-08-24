import Data.Char
import Numeric

main :: IO ()
main = do
  n <- readLn @Int
  print . (* 2) . read @Int $ showIntAtBase 5 intToDigit (n - 1) ""
