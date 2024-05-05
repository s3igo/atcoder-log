open Core

let read_line () = In_channel.(input_line_exn stdin)
let encode l = List.(group l ~break:Poly.( <> ) >>| fun l -> (length l, hd_exn l))
let cond b = if b then "Yes" else "No"

let () =
  let cnts l = List.(sort l ~compare:Poly.compare |> encode >>| fst) in
  read_line () |> String.to_list |> cnts |> cnts
  |> List.for_all ~f:(( = ) 2)
  |> cond |> print_endline
