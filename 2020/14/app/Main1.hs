module Main1 where

import Data.Map (Map, fromList, insert)
import Data.Text.Internal.Read (digitToInt)
import Text.Regex.PCRE

data Command = Mask String | Mem Int String deriving Show

zeroPad :: Int -> [Char] -> [Char]
zeroPad m xs = let ys = take m xs in replicate (m - length ys) '0' ++ ys

fromDecimal :: Int -> [Int]
fromDecimal 0 = [0]
fromDecimal n = mod n 2 : fromDecimal (div n 2)

decToBin :: Int -> String
decToBin = zeroPad 36.reverse.fmap (head.show).fromDecimal

binToDec :: [Char] -> Int
binToDec = foldl (\acc x -> acc * 2 + digitToInt x) 0

parse :: String -> Command
parse ('m':'a':'s':'k':' ':'=':' ':xs) = Mask xs
parse s =
    let [_:n:v:_] = s =~ "^mem\\[([0-9]+)\\] = ([0-9]+)$" :: [[String]]
    in Mem (read n) (decToBin $ read v)

applyMaskChar :: Char -> Char -> Char
applyMaskChar 'X' rc = rc
applyMaskChar mc _ = mc

apply :: (Map Int String, String) -> Command -> (Map Int String, String)
apply (r, _) (Mask m) = (r, m)
apply (r, m) (Mem n s) = (insert n (zipWith applyMaskChar m s) r, m)

main :: IO ()
main = do
    input <- lines <$> getContents
    print.sum.fmap binToDec.fst.foldl apply (fromList [], "").fmap parse $ input
