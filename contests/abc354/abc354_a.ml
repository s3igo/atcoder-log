open Core
open Scanf

let () =
  scanf "%d" @@ fun h ->
  Iter.(
    init (fun i -> Int.(2 ** i))
    |> zip_i
    |> fold_while
         (fun acc (i, cur) -> if acc + cur > h then (i + 1, `Stop) else (acc + cur, `Continue))
         0)
  |> printf "%d\n"
