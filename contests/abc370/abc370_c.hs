import Data.List

update :: Int -> a -> [a] -> [a]
update i x xs = take i xs ++ [x] ++ drop (i + 1) xs

iterate1 :: (a -> a) -> a -> [a]
iterate1 f x = f x : iterate1 f (f x)

main :: IO ()
main = do
  s <- getLine
  t <- getLine
  let n = length . filter id $ zipWith (/=) s t
  print n
  let step str = minimum [update i (t !! i) str | i <- findIndices id $ zipWith (/=) str t]
  mapM_ putStrLn . take n $ iterate1 step s
