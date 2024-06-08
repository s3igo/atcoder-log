open Core
open Poly

let read_line () = In_channel.(input_line_exn stdin)

let count l =
  List.fold l ~init:Char.Map.empty ~f:(fun acc x ->
      Map.update acc x ~f:(function None -> 1 | Some cnt -> cnt + 1))

let () =
  let s = read_line () |> String.to_list in
  let target =
    s |> count |> Map.to_alist
    |> List.sort ~compare:(fun (_, a) (_, b) -> Int.compare a b)
    |> List.hd_exn |> fst
  in
  s
  |> List.find_mapi ~f:(fun i x -> if x = target then Some (i + 1) else None)
  |> Option.iter ~f:(printf "%d\n")
