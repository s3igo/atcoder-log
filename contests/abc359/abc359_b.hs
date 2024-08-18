import Data.List.Split

main :: IO ()
main = print . length . filter (\[l, _, r] -> l == r) . divvy 3 1 . map (read @Int) . words =<< (getLine >> getLine)
