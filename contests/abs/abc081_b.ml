open Core

let read_line () = In_channel.(input_line_exn stdin)

let iter aa =
  let rec aux acc aa =
    if List.for_all ~f:(fun a -> a % 2 = 0) aa then
      aux (acc + 1) (List.map ~f:(fun a -> a / 2) aa)
    else acc
  in
  aux 0 aa

let () =
  let _ = read_line () in
  read_line () |> String.split ~on:' ' |> List.map ~f:Int.of_string |> iter
  |> printf "%d\n"
