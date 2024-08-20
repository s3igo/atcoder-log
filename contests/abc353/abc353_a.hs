import Data.List

main :: IO ()
main = do
  (h : hs) <- map (read @Int) . words <$> (getLine >> getLine)
  print . maybe (-1) (+ 2) $ findIndex (> h) hs
