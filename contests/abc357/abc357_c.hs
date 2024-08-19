import Data.Array.IArray
import Data.List.Split

flatMatrix :: (Show a) => Array (Int, Int, Int, Int) a -> Array (Int, Int) a
flatMatrix m = listArray ((a, b), (c'' * a'', d'' * b'')) swapped
  where
    ((a, b, c, d), (a', b', c', d')) = bounds m
    (a'', b'', c'', d'') = (a' - a + 1, b' - b + 1, c' - c + 1, d' - d + 1)
    swapped = [m ! (i, k, j, l) | i <- [a .. a'], j <- [c .. c'], k <- [b .. b'], l <- [d .. d']]

concatMatrix :: Array (Int, Int) (Array (Int, Int) a) -> Array (Int, Int, Int, Int) a
concatMatrix m = listArray bounds' [m ! (i, j) ! (k, l) | (i, j) <- indices m, (k, l) <- indices (m ! (i, j))]
  where
    ((a, b), (a', b')) = bounds m
    ((c, d), (c', d')) = bounds $ m ! (a, b)
    bounds' = ((a, b, c, d), (a', b', c', d'))

carpet :: Int -> Array (Int, Int) Char
carpet 0 = listArray ((1, 1), (1, 1)) "#"
carpet n = flatMatrix $ arranged // [((2, 2, i, j), '.') | i <- [c .. c'], j <- [d .. d']]
  where
    arranged = concatMatrix . listArray @Array ((1, 1) :: (Int, Int), (3, 3)) $ replicate 9 $ carpet (n - 1)
    ((_, _, c, d), (_, _, c', d')) = bounds arranged

main :: IO ()
main = readLn @Int >>= \n -> mapM_ putStrLn . chunksOf (3 ^ n) . elems $ carpet n
