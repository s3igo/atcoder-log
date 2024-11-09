import Data.List

main :: IO ()
main = do
  [a1, a2, a3, a4] <- sort . map (read @Int) . words <$> getLine
  print @Int $
    if a1 == a2
      then if a3 == a4 then 2 else 1
      else if a2 == a3 || a3 == a4 then 1 else 0
