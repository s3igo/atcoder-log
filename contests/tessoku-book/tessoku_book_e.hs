import Data.Functor ((<&>))
import Data.Ix (inRange)
import Flow

main :: IO ()
main = do
  [n, k] <- getLine <&> words .> map (read @Int)
  (+) <$> [1 .. n] <*> [1 .. n] |> map (k -) |> filter (inRange (1, n)) |> length |> print
