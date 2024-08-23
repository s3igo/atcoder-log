import Data.IntMap.Strict qualified as IM

main :: IO ()
main = do
  as <- IM.fromList . flip zip [1 :: Int ..] . map (read @Int) . words <$> (getLine >> getLine)
  let solve acc k = maybe (reverse acc) (\v -> solve (v : acc) v) $ IM.lookup k as
  putStrLn . unwords . map show $ solve [] (-1)
