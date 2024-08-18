{-# LANGUAGE OverloadedStrings #-}

import Data.Bool (bool)
import Data.ByteString.Char8 qualified as BS
import Data.List.Split (divvy)

solve :: [BS.ByteString] -> String
solve = bool "Yes" "No" . any p . divvy 2 1 . init
  where
    p [a, b] = a == "sweet" && b == "sweet"
    p _ = undefined

main :: IO ()
main = getLine >> BS.lines <$> BS.getContents >>= putStrLn . solve
