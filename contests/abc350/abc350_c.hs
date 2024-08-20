import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List
import Data.List.Extra (allSame)
import Data.Tuple (swap)

readInts :: IO [Int]
readInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main :: IO ()
main = do
  [n] <- readInts
  as <- listArray @UArray (1, n) <$> readInts
  let table = array @UArray (1, n) . map swap $ assocs as
  let step (ary, tbl, i) =
        let idx = tbl ! i
            elm = ary ! i
         in if i < n then Just ([i, idx], (ary // [(i, i), (idx, elm)], tbl // [(i, i), (elm, idx)], i + 1)) else Nothing
  let result = filter (not . allSame) $ unfoldr step (as, table, 1)
  print (length result) >> mapM_ (BS.putStrLn . BS.unwords . map (BS.pack . show)) result

-- TODO: TLE
