open Core
open Scanf

let ( >> ) f g x = g (f x)
let int_list_to_string = List.map ~f:string_of_int >> String.concat ~sep:" "

let solve l l' =
  let rec aux cnt acc = function
    | [], _ | _, [] -> List.rev acc
    | (x :: xs as xl), y :: ys ->
        if Poly.equal x y then aux (cnt + 1) (cnt :: acc) (xs, ys) else aux (cnt + 1) acc (xl, ys)
  in
  aux 1 [] (l, l')

let () =
  scanf "%s %s" @@ fun s t ->
  solve (String.to_list s) (String.to_list t) |> int_list_to_string |> print_endline
