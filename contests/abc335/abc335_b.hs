import Control.Monad (replicateM)

main :: IO ()
main = do
  n <- readLn @Int
  mapM_ (putStrLn . unwords . map show) . filter ((n >=) . sum) $ replicateM 3 [0 .. n]
