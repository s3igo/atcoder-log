import Data.Function
import Data.List

main :: IO ()
main = putChar . head . maximumBy (compare `on` length) . reverse . group . sort =<< getLine
