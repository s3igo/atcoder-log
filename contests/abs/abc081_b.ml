open Core

let read_line () = In_channel.(input_line_exn stdin)

let iter xs =
  let rec aux acc xs =
    if List.for_all ~f:(fun x -> x % 2 = 0) xs then
      aux (acc + 1) (List.map ~f:(fun x -> x / 2) xs)
    else acc
  in
  aux 0 xs

let () =
  let _ = read_line () in
  read_line () |> String.split ~on:' ' |> List.map ~f:Int.of_string |> iter
  |> printf "%d\n"
