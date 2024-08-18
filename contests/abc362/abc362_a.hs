{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List

main :: IO ()
main = do
  [r, g, b] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  BS.getLine
    >>= print . \case
      "Red" -> min g b
      "Green" -> min r b
      "Blue" -> min r g
      _ -> undefined
