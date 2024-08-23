import Data.List.Extra

main :: IO ()
main = do
  s <- getLine
  putStrLn $ if map head (group s) `elem` subsequences "ABC" then "Yes" else "No"
