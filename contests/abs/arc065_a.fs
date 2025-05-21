let bool f t cond = if cond then t else f

stdin.ReadLine()
|> fun s -> System.Text.RegularExpressions.Regex.IsMatch(s, "^(dream|dreamer|erase|eraser)+$")
|> bool "NO" "YES"
|> printfn "%s"
