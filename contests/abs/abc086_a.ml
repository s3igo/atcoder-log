open Core
open Scanf

let () =
  scanf "%d %d" @@ fun a b ->
  (if a * b % 2 = 0 then "Even" else "Odd") |> print_endline
