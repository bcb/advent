module Main2 where

import Data.Map (Map, fromList, insert)
import Data.Text.Internal.Read (digitToInt)
import Text.Regex.PCRE

data Command = Mask String | Mem Int String deriving (Show)

zeroPad :: Int -> [Char] -> [Char]
zeroPad m xs = let ys = take m xs in replicate (m - length ys) '0' ++ ys

fromDecimal :: Int -> [Int]
fromDecimal 0 = []
fromDecimal n = mod n 2 : fromDecimal (div n 2)

decToBin :: Int -> String
decToBin 0 = "0"
decToBin n = reverse . fmap (head . show) . fromDecimal $ n

binToDec :: [Char] -> Int
binToDec = foldl (\acc x -> acc * 2 + digitToInt x) 0

parse :: String -> Command
parse ('m' : 'a' : 's' : 'k' : ' ' : '=' : ' ' : xs) = Mask xs
parse s =
  let [_ : n : v : _] = s =~ "^mem\\[([0-9]+)\\] = ([0-9]+)$" :: [[String]]
   in Mem (read n) (zeroPad 36 . decToBin $ read v)

applyMaskChar :: Char -> Char -> Char
applyMaskChar '0' rc = rc
applyMaskChar mc _ = mc

merge :: String -> String -> String
merge s [] = s
merge (x : xs) (p : ps)
  | (x == 'X') = p : merge xs ps
  | otherwise = x : merge xs (p : ps)

permutations :: String -> [String]
permutations s =
  let totalxs = length . filter (== 'X') $ s
      totalAddresses = 2 ^ totalxs
   in fmap ((merge s) . zeroPad totalxs . decToBin) $ [0 .. totalAddresses -1]

update :: String -> Map Int String -> String -> Map Int String
update value registers address = insert (binToDec address) value registers

apply :: (Map Int String, String) -> Command -> (Map Int String, String)
apply (registers, _) (Mask m) = (registers, m)
apply (registers, mask) (Mem address value) =
  let addresses = (zipWith applyMaskChar mask) . zeroPad 36 . decToBin $ address
   in (foldl (update value) registers . permutations $ addresses, mask)

main :: IO ()
main = do
  input <- lines <$> getContents
  print . sum . fmap binToDec . fst . foldl apply (fromList [], "") . fmap parse $ input
