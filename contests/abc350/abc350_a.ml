open Core
open Scanf

let contains ~lower ~upper x = lower <= x && x <= upper

let () =
  (scanf "ABC%d" @@ function
   | 316 -> "No"
   | n when contains n ~lower:1 ~upper:349 -> "Yes"
   | _ -> "No")
  |> print_endline
