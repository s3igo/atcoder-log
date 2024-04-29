open Core

let read_line () = In_channel.(input_line_exn stdin)

let () =
  let s = read_line () in
  Str.(
    regexp {|^\(dream\|dreamer\|erase\|eraser\)+$|} |> fun pat ->
    if string_match pat s 0 then "YES" else "NO")
  |> print_endline
