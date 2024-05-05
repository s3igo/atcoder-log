open Core

let read_line () = In_channel.(input_line_exn stdin)
let cond b = if b then "YES" else "NO"

let () =
  Str.(string_match (regexp {|^\(dream\|dreamer\|erase\|eraser\)+$|})) (read_line ()) 0
  |> cond |> print_endline
