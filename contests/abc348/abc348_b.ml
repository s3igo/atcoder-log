open Core

let read_line () = In_channel.(input_line_exn stdin)
let read_lines () = In_channel.(input_lines stdin)
let string_to_int_list s = String.split ~on:' ' s |> List.map ~f:int_of_string
let list_to_tuple2 = function [ a; b ] -> (a, b) | _ -> assert false
let diff (x1, y1) (x2, y2) = Int.pow (x1 - x2) 2 + Int.pow (y1 - y2) 2

let () =
  let _ = read_line () in
  let xy =
    read_lines ()
    |> List.map ~f:(fun s -> string_to_int_list s |> list_to_tuple2)
  in
  List.(
    map xy ~f:(fun p -> (p, xy)) >>| fun ((x0, y0), l) ->
    mapi l ~f:(fun i e -> (i + 1, e))
    |> fold ~init:(0, 0) ~f:(fun acc (i, (x, y)) ->
           match diff (x0, y0) (x, y) with
           | d when d > snd acc -> (i, d)
           | _ -> acc))
  |> List.iter ~f:(fun (i, _) -> printf "%d\n" i)
