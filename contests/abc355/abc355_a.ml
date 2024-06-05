open Core
open Scanf

let () = scanf "%d %d" @@ fun a b -> (if a = b then -1 else 6 - a - b) |> printf "%d\n"
