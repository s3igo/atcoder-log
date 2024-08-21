import Data.List

main :: IO ()
main = mapM_ (putStrLn . unwords . map (show . succ) . elemIndices "1" . words) . tail . lines =<< getContents
