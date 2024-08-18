import Data.Bool
import Data.List
import Data.List.Split

main :: IO ()
main = do
  [s, t] <- words <$> getLine
  putStrLn . bool "No" "Yes" $ t `elem` concat [transpose $ chunksOf w s | w <- [1 .. length s - 1]]
