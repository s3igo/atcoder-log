open Core
open Scanf

let () =
  scanf "%d %d %d %d" @@ fun n x y z ->
  (if min x y <= z && z <= max x y then "Yes" else "No") |> print_endline
