open Core

let read_line () = In_channel.(input_line_exn stdin)

let encode lst =
  List.(group ~break:Poly.( <> ) lst >>| fun l -> (length l, hd_exn l))

let () =
  let s = read_line () in
  List.(
    let cnt l = sort ~compare:Poly.compare l |> encode >>| fst in
    cnt (String.to_list s) |> cnt |> for_all ~f:(fun x -> x = 2))
  |> fun b -> (if b then "Yes" else "No") |> print_endline
