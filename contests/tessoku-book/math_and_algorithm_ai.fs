let [| [| _; q |]; a |] =
    (fun _ -> stdin.ReadLine().Split() |> Array.map int) |> Array.init 2

let cumsum = Array.scan (+) 0 a

(fun _ -> stdin.ReadLine().Split() |> Array.map int)
|> Array.init q
|> Array.map (fun [| l; r |] -> cumsum[r] - cumsum[l - 1] |> printfn "%d")
