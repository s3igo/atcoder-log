main :: IO ()
main = print . length . filter (== "Takahashi") . lines =<< (getLine >> getContents)
