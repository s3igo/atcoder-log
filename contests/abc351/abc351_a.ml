open Core

let read_lines () = In_channel.(input_lines stdin)
let sum s = String.split ~on:' ' s |> List.sum (module Int) ~f:int_of_string

let () =
  (match read_lines () |> List.map ~f:sum with
  | [ a; b ] -> a - b + 1
  | _ -> assert false)
  |> printf "%d\n"
