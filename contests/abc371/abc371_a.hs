f :: String -> String -> String -> String
f "<" ">" _ = "A"
f ">" "<" _ = "A"
f _ "<" ">" = "C"
f _ ">" "<" = "C"
f "<" _ "<" = "B"
f ">" _ ">" = "B"
f _ _ _ = undefined

main :: IO ()
main = do
  [ab, ac, bc] <- words <$> getLine
  putStrLn $ f ab ac bc
