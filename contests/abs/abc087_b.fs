open FSharpPlus.Operators

let [| a; b; c; x |] = stdin.ReadToEnd().Split()[0..3] |> Array.map uint64

(fun i j k -> 500UL * i + 100UL * j + 50UL * k) <!> [| 0UL .. a |]
<*> [| 0UL .. b |]
<*> [| 0UL .. c |]
|> Array.filter ((=) x)
|> Array.length
|> printfn "%d"
