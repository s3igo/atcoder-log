open Core

let read_line () = In_channel.(input_line_exn stdin)
let enumerate lst = List.(zip_exn (range 0 (length lst)) lst)

let () =
  let _ = read_line () in
  let a = read_line () |> String.split ~on:' ' |> List.map ~f:int_of_string in
  List.(
    sort ~compare:Int.descending a
    |> enumerate
    |> partition_tf ~f:(fun (i, _) -> i % 2 = 0))
  |> fun (a, b) ->
  let snd_sum lst = List.sum (module Int) ~f:snd lst in
  snd_sum a - snd_sum b |> printf "%d\n"
