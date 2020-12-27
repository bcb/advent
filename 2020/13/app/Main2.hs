module Main2 where

import Data.Maybe (isJust)
import Data.List.Split (splitOn)
import Data.Tuple (swap)
import Math.NumberTheory.Moduli.Chinese (chineseRemainder)

main :: IO ()
main = do
    input <- lines <$> readFile "testinput"
    print.fmap (\(x, y) -> (x, read y :: Integer)).filter (\(_, y) -> y /= "x") $ zip [0..] $ (splitOn ",").head.tail $ input
