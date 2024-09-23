import Control.Monad (foldM_, replicateM)

update :: Int -> a -> [a] -> [a]
update i x xs = take i xs ++ [x] ++ drop (i + 1) xs

step :: [Bool] -> (Int, Char) -> IO [Bool]
step xs (a, 'M') | xs !! a = putStrLn "Yes" >> return (update a False xs)
step xs _ = putStrLn "No" >> return xs

main :: IO ()
main = do
  [n, m] <- map (read @Int) . words <$> getLine
  foldM_ step (replicate n True) =<< replicateM m (do [a, b] <- words <$> getLine; return (pred $ read @Int a, head b))
