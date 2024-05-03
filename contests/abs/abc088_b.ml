open Core

let read_line () = In_channel.(input_line_exn stdin)

let () =
  let _ = read_line () in
  let a = read_line () |> String.split ~on:' ' |> List.map ~f:int_of_string in
  List.(
    sort ~compare:Int.descending a
    |> mapi ~f:(fun i x -> (i, x))
    |> partition_tf ~f:(fun (i, _) -> i % 2 = 0)
    |> fun (a, b) ->
    let snd_sum = sum (module Int) ~f:snd in
    snd_sum a - snd_sum b)
  |> printf "%d\n"
