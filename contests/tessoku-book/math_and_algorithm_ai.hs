import Control.Monad (replicateM)
import Data.Array.Unboxed (UArray, listArray, (!))
import Data.Functor ((<&>))
import Data.List qualified as L
import Flow

main :: IO ()
main = do
  [[n, q], as] <- getLine <&> words .> map (read @Int) |> replicateM 2
  let cumsum = as |> L.scanl' (+) 0 |> listArray @UArray (0, n)
  (getLine <&> words .> map (read @Int) |> replicateM q) >>= mapM_ (\[l, r] -> (cumsum ! r) - (cumsum ! pred l) |> print)
