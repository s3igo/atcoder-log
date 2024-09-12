import Data.List.Extra (nubOrd)

main :: IO ()
main = print . length . nubOrd . map (read @Int) . lines =<< (getLine >> getContents)
