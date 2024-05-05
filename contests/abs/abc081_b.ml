open Core

let read_line () = In_channel.(input_line_exn stdin)
let ( >> ) f g x = g (f x)
let string_to_int_list = String.split ~on:' ' >> List.map ~f:int_of_string

let solve l =
  let rec aux acc l =
    if List.for_all l ~f:(fun x -> x % 2 = 0) then aux (acc + 1) (List.map l ~f:(fun x -> x / 2))
    else acc
  in
  aux 0 l

let () =
  let _ = read_line () in
  read_line () |> string_to_int_list |> solve |> printf "%d\n"
