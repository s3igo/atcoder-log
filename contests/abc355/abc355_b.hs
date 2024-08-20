import Data.Bool
import Data.List
import Data.List.Split

main :: IO ()
main = do
  a <- map ((,True) . read @Int) . words <$> (getLine >> getLine)
  b <- map ((,False) . read @Int) . words <$> getLine
  putStrLn . bool "No" "Yes" . any (foldl1' (&&)) . divvy 2 1 . map snd . sort $ a ++ b
