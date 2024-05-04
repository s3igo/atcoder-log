open Core
open Scanf

let contains ~lower ~upper x = lower <= x && x <= upper

let () =
  scanf "%d %d %d %d" @@ fun n x y z ->
  (if contains ~lower:(min x y) ~upper:(max x y) z then "Yes" else "No") |> print_endline
