open Core

let read_line () = In_channel.(input_line_exn stdin)
let read_lines () = In_channel.(input_lines stdin)
let ( >> ) f g x = g (f x)
let string_to_int_list = String.split ~on:' ' >> List.map ~f:int_of_string
let int_list_to_string = List.map ~f:string_of_int >> String.concat ~sep:" "

let () =
  let _ = read_line () in
  List.(
    read_lines () >>| string_to_int_list
    >>| filter_mapi ~f:(fun i x -> if x = 1 then Some (i + 1) else None)
    >>| int_list_to_string |> iter ~f:print_endline)
