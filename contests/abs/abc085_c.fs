let [| n; y |] = stdin.ReadLine().Split() |> Array.map int64

let cands =
    [| for i in 0L .. n do
           for j in 0L .. n - i -> i, j, n - i - j |]

cands
|> Array.tryFind (fun (i, j, k) -> 10000L * i + 5000L * j + 1000L * k = y)
|> Option.defaultValue (-1, -1, -1)
|||> printfn "%d %d %d"
