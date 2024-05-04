open Core

let ( >> ) f g x = g (f x)
let read_line () = In_channel.(input_line_exn stdin)
let int_list_to_string = List.map ~f:string_of_int >> String.concat ~sep:" "

let solve l l' =
  let rec aux l l' cnt acc =
    match (l, l') with
    | (x :: xs as xl), y :: ys ->
        if Poly.equal x y then aux xs ys (cnt + 1) (cnt :: acc) else aux xl ys (cnt + 1) acc
    | _ -> acc
  in
  aux l l' 1 [] |> List.rev

let () =
  let s = read_line () in
  let t = read_line () in
  solve (String.to_list s) (String.to_list t) |> int_list_to_string |> print_endline
