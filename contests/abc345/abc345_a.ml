open Core

let read_line () = In_channel.(input_line_exn stdin)
let cond b = if b then "Yes" else "No"
let () = Str.(string_match (regexp {|^<=+>$|})) (read_line ()) 0 |> cond |> print_endline
