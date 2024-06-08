open Core

let read_lines () = In_channel.(input_lines stdin)
let () = read_lines () |> List.rev |> List.iter ~f:print_endline
