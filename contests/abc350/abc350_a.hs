import Data.Char (isAlpha)

contains :: (Int, Int) -> Int -> Bool
(l, u) `contains` n = l <= n && n <= u

main :: IO ()
main = do
  s <- read @Int . dropWhile isAlpha <$> getLine
  putStrLn $ if (1, 349) `contains` s && s /= 316 then "Yes" else "No"
