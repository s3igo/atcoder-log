open Core
open Poly
open Scanf

let () =
  scanf "%s %s" @@ fun s t ->
  let s = String.to_list s in
  for w = 1 to List.length s - 1 do
    for c = 1 to w do
      let tgt =
        s
        |> List.groupi ~break:(fun i _ _ -> i % w = 0)
        |> List.filter_map ~f:(fun l -> List.nth l (c - 1))
        |> String.of_list
      in
      if tgt = t then (
        print_endline "Yes";
        exit 0)
    done
  done;
  print_endline "No"
