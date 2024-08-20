main :: IO ()
main = print . negate . sum . map (read @Int) . words =<< (getLine >> getLine)
