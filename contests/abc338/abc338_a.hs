import Data.Char

main :: IO ()
main = do
  (x : xs) <- getLine
  putStrLn $ if isUpper x && all isLower xs then "Yes" else "No"
