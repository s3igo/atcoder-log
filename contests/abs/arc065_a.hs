import Data.ByteString.Char8 qualified as BS
import Text.Regex.TDFA ((=~))

main :: IO ()
main = do
  s <- BS.getLine
  putStrLn $ if s =~ "^(dream|dreamer|erase|eraser)+$" then "YES" else "NO"
