open Core
open Scanf

let () =
  scanf "%d %d" @@ fun n y ->
  Iter.(
    0 -- n >>= fun i ->
    0 -- (n - i) >>= fun j -> return (i, j, n - i - j))
  |> Iter.find_pred (fun (i, j, k) -> (10000 * i) + (5000 * j) + (1000 * k) = y)
  |> Option.value ~default:(-1, -1, -1)
  |> fun (i, j, k) -> printf "%d %d %d\n" i j k
