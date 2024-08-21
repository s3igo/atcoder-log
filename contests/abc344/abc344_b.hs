main :: IO ()
main = mapM_ putStrLn . reverse . lines =<< getContents
