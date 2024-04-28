open Core
open Scanf

let () =
  scanf "%d %d %d %d" @@ fun a b c x ->
  Iter.(
    0 -- a >>= fun i ->
    0 -- b >>= fun j ->
    0 -- c >>= fun k -> return (i, j, k))
  |> Iter.filter_count (fun (i, j, k) -> (500 * i) + (100 * j) + (50 * k) = x)
  |> printf "%d\n"
