import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List
import Data.Maybe (fromMaybe)

readInts :: IO [Int]
readInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main :: IO ()
main = do
  [n, _] <- readInts
  let step (horz, vert, backSlash, slash) a =
        let (i, j) = a `divMod` n
            horz' = horz // [(i, horz ! i + 1)]
            vert' = vert // [(j, vert ! j + 1)]
            backSlash' = if i == j then backSlash + 1 else backSlash
            slash' = if i + j == n - 1 then slash + 1 else slash
         in (horz', vert', backSlash', slash')
  let initialAry = listArray @UArray (0, n - 1) $ repeat 0
  reduced <- scanl' step (initialAry, initialAry, 0, 0) . map pred <$> readInts
  let aux (horz, vert, backSlash, slash) = elem n $ elems horz ++ elems vert ++ [backSlash, slash]
  print . fromMaybe (-1) $ findIndex aux reduced
