{-# LANGUAGE LambdaCase #-}

import Data.Functor ((<&>))
import Flow

main :: IO ()
main = do
  [n, y] <- getLine <&> words .> map (read @Int)
  [[i, j, k] | i <- [0 .. n], j <- [0 .. n - i], let k = n - i - j, 10000 * i + 5000 * j + 1000 * k == y]
    |> (\case [] -> "-1 -1 -1"; x : _ -> x |> map show |> unwords)
    |> putStrLn
