import Data.List

main :: IO ()
main = do
  [a, b, c] <- sort <$> getLine
  putStrLn $ if a == 'A' && b == 'B' && c == 'C' then "Yes" else "No"
