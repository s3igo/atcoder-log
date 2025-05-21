import Data.Functor ((<&>))
import Flow

main :: IO ()
main = do
  [a, b, c, x] <- getContents <&> lines .> map (read @Int)
  (\i j k -> 500 * i + 100 * j + 50 * k) <$> [0 .. a] <*> [0 .. b] <*> [0 .. c] |> filter (== x) |> length |> print
