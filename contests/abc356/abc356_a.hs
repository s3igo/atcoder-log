import Data.Vector.Algorithms.Intro qualified as VA
import Data.Vector.Unboxed qualified as VU

main :: IO ()
main = do
  [n, l, r] <- map (read @Int) . words <$> getLine
  putStrLn . unwords . map show . VU.toList . VU.modify (\v -> VA.sortByBounds (flip $ compare @Int) v (l - 1) r) $ VU.enumFromN 1 n
