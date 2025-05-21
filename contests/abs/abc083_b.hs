import Data.Functor ((<&>))
import Data.Ix (inRange)
import Flow

digits :: Int -> [Int]
digits 0 = []
digits n = m : digits d
  where
    (d, m) = n `divMod` 10

main :: IO ()
main = do
  [n, a, b] <- getLine <&> words .> map (read @Int)
  [1 .. n] |> filter (digits .> sum .> inRange (a, b)) |> sum |> print
