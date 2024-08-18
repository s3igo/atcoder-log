import Control.Monad

main :: IO ()
main = do
  [_, a] <- readInts
  let aux acc t = let newAcc = max acc t + a in print newAcc >> return newAcc
  foldM_ aux 0 =<< readInts
  where
    readInts = map (read @Int) . words <$> getLine
