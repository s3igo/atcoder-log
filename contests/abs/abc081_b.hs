import Flow

solve :: [Int] -> Int
solve xs
  | xs |> all even = xs |> map (`div` 2) |> solve |> (+ 1)
  | otherwise = 0

main :: IO ()
main = getLine >> getLine >>= words .> map (read @Int) .> solve .> print
