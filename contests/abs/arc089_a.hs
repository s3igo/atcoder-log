import Control.Monad (replicateM)

solve :: (Int, Int, Int) -> [(Int, Int, Int)] -> Bool
solve _ [] = True
solve (t0, x0, y0) ((t, x, y) : txys) = d <= dt && even (dt - d) && solve (t, x, y) txys
  where
    dt = t - t0
    dx = abs $ x - x0
    dy = abs $ y - y0
    d = dx + dy

main :: IO ()
main = do
  n <- readLn @Int
  txys <- replicateM n $ do [t, x, y] <- map (read @Int) . words <$> getLine; return (t, x, y)
  putStrLn $ if solve (0, 0, 0) txys then "Yes" else "No"
