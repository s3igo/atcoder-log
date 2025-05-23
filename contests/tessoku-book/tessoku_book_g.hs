import Control.Monad (replicateM)
import Data.Array.Unboxed (UArray, elems, listArray, (!), (//))
import Data.Functor ((<&>))
import Data.List qualified as L
import Flow

folder :: UArray Int Int -> (Int, Int) -> UArray Int Int
folder acc (l, r) = acc // [(l, acc ! l + 1), (r + 1, acc ! (r + 1) - 1)]

main :: IO ()
main = do
  [d, n] <- readLn @Int |> replicateM 2
  let initial = replicate (d + 1) (0 :: Int) |> listArray @UArray (1, d + 1)
  ((do [l, r] <- getLine <&> words .> map (read @Int); pure (l, r)) |> replicateM n)
    >>= L.foldl' folder initial .> elems .> L.scanl' (+) 0 .> tail .> init .> mapM_ print
