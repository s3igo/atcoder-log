open Core

let read_line () = In_channel.(input_line_exn stdin)
let string_to_int_list s = String.split ~on:' ' s |> List.map ~f:int_of_string

let () =
  let _ = read_line () |> int_of_string in
  let a = read_line () |> string_to_int_list in
  List.sum (module Int) ~f:Fn.id a |> ( - ) 0 |> printf "%d\n"
