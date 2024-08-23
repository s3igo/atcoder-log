import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.List
import Data.List.Split
import Data.Tuple.Extra

main :: IO ()
main = do
  [h, w, n] <- map (read @Int) . words <$> getLine
  let grid = listArray @UArray ((0, 0), (h - 1, w - 1)) $ repeat '.'
  let step (cur@(i, j), dir, g) _
        | g ! cur == '.' = let turned = (dir + 1) `mod` 4 in (next turned, turned, g // [(cur, '#')])
        | otherwise = let turned = (dir - 1) `mod` 4 in (next turned, turned, g // [(cur, '.')])
        where
          next d = case d of
            0 -> ((i - 1) `mod` h, j)
            1 -> (i, (j + 1) `mod` w)
            2 -> ((i + 1) `mod` h, j)
            3 -> (i, (j - 1) `mod` w)
            _ -> undefined
  mapM_ putStrLn . chunksOf w . elems . thd3 $ foldl' step ((0, 0), 0 :: Int, grid) [1 .. n]
