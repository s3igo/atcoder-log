let bool f t cond = if cond then t else f

let x = stdin.ReadLine().Split()[1]
stdin.ReadLine().Split() |> Array.contains x |> bool "No" "Yes" |> printfn "%s"
