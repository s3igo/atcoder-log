open Core

let read_line () = In_channel.(input_line_exn stdin)
let read_lines () = In_channel.(input_lines stdin)
let ( >> ) f g x = g (f x)
let string_to_int_list = String.split ~on:' ' >> List.map ~f:int_of_string
let cond b = if b then "Yes" else "No"

let () =
  let _ = read_line () in
  let a = read_line () |> string_to_int_list in
  List.(
    read_lines () >>| string_to_int_list |> transpose_exn
    >>| sum (module Int) ~f:Fn.id
    |> for_all2_exn a ~f:(fun a b -> a <= b))
  |> cond |> print_endline
