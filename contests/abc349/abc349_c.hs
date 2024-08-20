import Data.Bool (bool)
import Data.ByteString.Char8 qualified as BS
import Data.Char (toLower)
import Text.Regex.TDFA ((=~))

main :: IO ()
main = do
  s <- BS.getLine
  [a, b, c] <- map toLower <$> getLine
  putStrLn . bool "No" "Yes" $
    if c == 'x'
      then s =~ ("^.*" ++ [a] ++ ".*" ++ [b] ++ ".*$")
      else s =~ ("^.*" ++ [a] ++ ".*" ++ [b] ++ ".*" ++ [c] ++ ".*$")
