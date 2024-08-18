open Core
open Poly

let read_line () = In_channel.(input_line_exn stdin)
let read_lines () = In_channel.(input_lines stdin)

let () =
  let _ = read_line () in
  read_lines () |> List.count ~f:(( = ) "Takahashi") |> printf "%d\n"
