open Core

let read_line () = In_channel.(input_line_exn stdin)
let ( >> ) f g x = g (f x)
let string_to_int_list = String.split ~on:' ' >> List.map ~f:int_of_string
let int_list_to_string = List.map ~f:string_of_int >> String.concat ~sep:" "

let windows l =
  let rec aux acc = function a :: (b :: _ as t) -> aux ((a, b) :: acc) t | _ -> List.rev acc in
  aux [] l

let () =
  let _ = read_line () in
  List.(read_line () |> string_to_int_list |> windows >>| fun (a, b) -> a * b)
  |> int_list_to_string |> print_endline
