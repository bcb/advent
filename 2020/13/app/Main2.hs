module Main2 where

import Data.List.Split (splitOn)
import Math.NumberTheory.Moduli.Chinese (chineseRemainder)

clean :: (Integer, String) -> (Integer, Integer)
clean (n, bstr) = let b = read bstr :: Integer in (b - n, b)

nonxs (_, b) = b /= "x"

main :: IO ()
main = do
    input <- lines <$> readFile "input"
    print.chineseRemainder.fmap clean.filter nonxs.zip [0..].splitOn ",".head.tail $ input
