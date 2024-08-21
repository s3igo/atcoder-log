import Data.List.Split (splitOn)

main :: IO ()
main = do
  [l, _, r] <- splitOn "|" <$> getLine
  putStrLn (l ++ r)
