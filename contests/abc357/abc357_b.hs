import Data.Char

main :: IO ()
main = do
  s <- getLine
  let lower = length $ filter isLower s
      upper = length s - lower
  putStrLn $ if upper > lower then map toUpper s else map toLower s
