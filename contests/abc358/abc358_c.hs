{-# LANGUAGE LambdaCase #-}

import Data.List

main :: IO ()
main = print . minimum . map length . filter p . subsequences . map oxToBool . lines =<< (getLine >> getContents)
  where
    oxToBool = map $ \case 'o' -> True; 'x' -> False; _ -> undefined
    p = and . foldr @[] (zipWith (||)) (repeat False)
