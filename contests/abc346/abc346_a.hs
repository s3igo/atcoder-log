import Data.List.Split (divvy)

main :: IO ()
main = putStrLn . unwords . map (show . product) . divvy 2 1 . map (read @Int) . words =<< (getLine >> getLine)
