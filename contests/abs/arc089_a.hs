import Control.Monad (replicateM)
import Control.Monad.Extra (foldM)
import Data.Bool (bool)
import Data.Maybe (isJust)
import Flow

folder :: (Int, Int, Int) -> (Int, Int, Int) -> Maybe (Int, Int, Int)
folder (t0, x0, y0) (t, x, y) = if d <= dt && (dt - d |> even) then Just (t, x, y) else Nothing
  where
    dt = t - t0
    dx = x - x0 |> abs
    dy = y - y0 |> abs
    d = dx + dy

main :: IO ()
main = do
  n <- readLn @Int
  (getLine >>= words .> map (read @Int) .> \[t, x, y] -> pure (t, x, y) |> replicateM n)
    >>= foldM folder (0, 0, 0) .> isJust .> bool "No" "Yes" .> putStrLn
