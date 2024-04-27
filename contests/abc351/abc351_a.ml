open Core

let read_lines () = In_channel.(input_lines stdin)

let sum s =
  s |> String.split ~on:' '
  |> List.fold ~init:0 ~f:(fun acc x -> acc + Int.of_string x)

let () =
  (match read_lines () |> List.map ~f:sum with
  | [ a; b ] -> a - b + 1
  | _ -> assert false)
  |> printf "%d\n"
