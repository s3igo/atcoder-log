import Data.Bits

main :: IO ()
main = do
  [l, r] <- map (read @Int) . words <$> getLine
  putStrLn $ if l .^. r == 1 then if l == 1 then "Yes" else "No" else "Invalid"
