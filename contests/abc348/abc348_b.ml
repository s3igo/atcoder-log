open Core

let read_line () = In_channel.(input_line_exn stdin)
let read_lines () = In_channel.(input_lines stdin)
let ( >> ) f g x = g (f x)
let string_to_int_list = String.split ~on:' ' >> List.map ~f:int_of_string
let list_to_tuple2 = function [ a; b ] -> (a, b) | _ -> assert false
let diff (x, y) (x', y') = Int.(((x - x') ** 2) + ((y - y') ** 2))

let () =
  let _ = read_line () in
  let xy = read_lines () |> List.map ~f:(string_to_int_list >> list_to_tuple2) in
  let max_idx p =
    List.foldi xy ~init:(0, 0) ~f:(fun i (idx, max) p' ->
        let d = diff p p' in
        if d > max then (i + 1, d) else (idx, max))
    |> fst
  in
  List.(xy >>| max_idx |> iter ~f:(printf "%d\n"))
