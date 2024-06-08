open Core

let read_line () = In_channel.(input_line_exn stdin)
let () = read_line () |> Str.(global_replace (regexp "|.*|") "") |> print_endline
