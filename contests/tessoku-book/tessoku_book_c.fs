open FSharpPlus.Operators

let bool f t cond = if cond then t else f

let [| [| _; k |]; ps; qs |] =
    fun _ -> stdin.ReadLine().Split() |> Array.map uint64
    |> Array.init 3

(+) <!> ps <*> qs |> Array.contains k |> bool "No" "Yes" |> printfn "%s"
