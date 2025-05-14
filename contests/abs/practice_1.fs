let a = stdin.ReadLine() |> uint64
let [| b; c |] = stdin.ReadLine().Split() |> Array.map uint64

printfn "%d %s" <| a + b + c <| stdin.ReadLine()
