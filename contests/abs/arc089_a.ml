open Core

let read_line () = In_channel.(input_line_exn stdin)
let read_lines () = In_channel.(input_lines stdin)
let string_to_int_list s = String.split ~on:' ' s |> List.map ~f:int_of_string
let list_to_tuple3 = function [ a; b; c ] -> (a, b, c) | _ -> assert false

let () =
  let _ = read_line () in
  let txy =
    read_lines ()
    |> List.map ~f:(fun l -> string_to_int_list l |> list_to_tuple3)
  in
  List.fold_until txy ~init:(0, 0, 0)
    ~f:(fun (t0, x0, y0) (t, x, y) ->
      let dt = t - t0 in
      let traveled = abs (x - x0) + abs (y - y0) in
      if traveled > dt || (dt - traveled) % 2 <> 0 then Stop false
      else Continue (t, x, y))
    ~finish:(fun _ -> true)
  |> fun cond -> (if cond then "Yes" else "No") |> print_endline
