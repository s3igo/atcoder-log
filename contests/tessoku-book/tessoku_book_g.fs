open FSharpPlus.Operators

let folder (acc: int64 array) [| l; r |] =
    acc[l] <- acc[l] + 1L
    acc[r + 1] <- acc[r + 1] - 1L
    acc

let [| d; n |] = (fun _ -> stdin.ReadLine() |> int) |> Array.init 2

fun _ -> stdin.ReadLine().Split() |> Array.map (int >> flip (-) 1)
|> Array.init n
|> Array.fold folder (Array.zeroCreate <| d + 1)
|> Array.scan (+) 0L
|> fun xs -> xs[1..d]
|> Array.iter (printfn "%d")
