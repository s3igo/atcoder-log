import Data.ByteString.Char8 qualified as BS

main :: IO ()
main = BS.getLine >>= print . BS.count '1'
