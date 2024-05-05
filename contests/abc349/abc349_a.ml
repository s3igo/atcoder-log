open Core

let read_line () = In_channel.(input_line_exn stdin)

let () =
  let _ = read_line () in
  read_line () |> String.split ~on:' '
  |> List.sum (module Int) ~f:int_of_string
  |> ( - ) 0 |> printf "%d\n"
