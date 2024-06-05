open Core

let read_line () = In_channel.(input_line_exn stdin)
let ( >> ) f g x = g (f x)
let string_to_int_list = String.split ~on:' ' >> List.map ~f:int_of_string
let cond b = if b then "Yes" else "No"

let windows l =
  let rec aux acc = function a :: (b :: _ as t) -> aux ((a, b) :: acc) t | _ -> List.rev acc in
  aux [] l

let () =
  let _ = read_line () in
  let read = read_line >> string_to_int_list >> Set.of_list (module Int) in
  let a = read () in
  let b = read () in
  Set.union a b |> Set.to_list |> windows
  |> List.exists ~f:(fun (l, r) -> Set.(mem a l && mem a r))
  |> cond |> print_endline
