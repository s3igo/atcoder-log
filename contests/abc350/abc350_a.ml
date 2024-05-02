open Core
open Scanf

let () =
  (scanf "ABC%d" @@ function
   | 316 -> "No"
   | n when 1 <= n && n <= 349 -> "Yes"
   | _ -> "No")
  |> print_endline
