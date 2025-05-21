import Data.Bool (bool)
import Data.ByteString.Char8 qualified as BS
import Flow
import Text.Regex.TDFA ((=~))

main :: IO ()
main = BS.getLine >>= flip (=~) "^(dream|dreamer|erase|eraser)+$" .> bool "NO" "YES" .> putStrLn
