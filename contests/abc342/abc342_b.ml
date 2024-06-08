open Core

let read_line () = In_channel.(input_line_exn stdin)
let read_lines () = In_channel.(input_lines stdin)
let ( >> ) f g x = g (f x)
let string_to_int_list = String.split ~on:' ' >> List.map ~f:int_of_string
let list_to_tuple2 = function [ a; b ] -> (a, b) | _ -> assert false

let () =
  let _ = read_line () in
  let idx =
    read_line () |> string_to_int_list
    |> List.mapi ~f:(fun i x -> (x, i))
    |> Map.of_alist_exn (module Int)
    |> Map.find_exn
  in
  let _ = read_line () in
  List.(
    read_lines () >>| string_to_int_list >>| list_to_tuple2
    >>| (fun (a, b) -> if idx a < idx b then a else b)
    |> iter ~f:(printf "%d\n"))
