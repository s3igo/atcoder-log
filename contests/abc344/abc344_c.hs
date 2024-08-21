import Control.Monad (forM_, replicateM)
import Data.IntSet qualified as IS

main :: IO ()
main = do
  [as, bs, cs, xs] <- replicateM 4 $ map (read @Int) . words <$> (getLine >> getLine)
  let set = IS.fromList [a + b + c | a <- as, b <- bs, c <- cs]
  forM_ xs $ \x -> putStrLn $ if IS.member x set then "Yes" else "No"
