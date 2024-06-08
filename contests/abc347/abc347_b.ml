open Core
open Scanf

let all_substrs s =
  let n = String.length s in
  let rec aux i acc =
    if i >= n then acc
    else
      let rec inner j acc' =
        if j > n then acc' else inner (j + 1) (Stdlib.String.sub s i (j - i) :: acc')
      in
      aux (i + 1) (inner (i + 1) acc)
  in
  aux 0 []

let () =
  scanf "%s" @@ all_substrs
  |> List.dedup_and_sort ~compare:Poly.compare
  |> List.length |> printf "%d\n"
