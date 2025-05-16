stdin.ReadLine()
|> fun s -> System.Text.RegularExpressions.Regex.IsMatch(s, "^(dream|dreamer|erase|eraser)+$")
|> fun b -> if b then "YES" else "NO"
|> printfn "%s"
