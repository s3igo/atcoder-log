let (|DivAll|_|) xs =
    if xs |> Array.forall System.UInt64.IsEvenInteger then
        xs |> Array.map (fun x -> x / 2UL) |> Some
    else
        None

let rec solve =
    function
    | DivAll xs -> xs |> solve |> (+) 1UL
    | _ -> 0UL

stdin.ReadLine() |> ignore
stdin.ReadLine().Split() |> Array.map uint64 |> solve |> printfn "%d"
