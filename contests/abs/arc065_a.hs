import Data.Bool (bool)
import Data.ByteString.Char8 qualified as BS
import Text.Regex.TDFA ((=~))

main :: IO ()
main = do
  s <- BS.getLine
  putStrLn . bool "NO" "YES" $ s =~ "^(dream|dreamer|erase|eraser)+$"
