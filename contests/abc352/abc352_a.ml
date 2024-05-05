open Core
open Scanf

let contains ~lower ~upper x = lower <= x && x <= upper
let cond b = if b then "Yes" else "No"

let () =
  scanf "%d %d %d %d" @@ fun n x y z ->
  contains z ~lower:(min x y) ~upper:(max x y) |> cond |> print_endline
