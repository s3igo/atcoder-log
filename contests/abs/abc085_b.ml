open Core

let read_line () = In_channel.(input_line_exn stdin)
let read_lines () = In_channel.(input_lines stdin)

let () =
  let _ = read_line () in
  List.(read_lines () >>| int_of_string |> dedup_and_sort ~compare:Int.compare |> length)
  |> printf "%d\n"
