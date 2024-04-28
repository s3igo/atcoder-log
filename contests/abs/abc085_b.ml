open Core

let read_line () = In_channel.(input_line_exn stdin)
let read_lines () = In_channel.(input_lines stdin)

let () =
  let _ = read_line () in
  let d = read_lines () |> List.map ~f:int_of_string in
  List.(dedup_and_sort ~compare:Int.compare d |> length) |> printf "%d\n"
