import Data.Char
import Data.List.Extra
import Numeric

main :: IO ()
main = do
  n <- readLn @Int
  print . length . takeWhileEnd (== '0') $ showIntAtBase 2 intToDigit n ""
