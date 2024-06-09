open Core
open Poly

let read_line () = In_channel.(input_line_exn stdin)
let contains ~l ~r x = l <= x && x <= r

let () =
  let s = read_line () in
  let lower, upper = s |> String.partition_tf ~f:(contains ~l:'a' ~r:'z') in
  (if String.length upper > String.length lower then String.uppercase s else String.lowercase s)
  |> print_endline
