import Data.List (elemIndex)
import Data.List.Split (splitOn)

getNext :: [Int] -> Int
getNext xs = go $ (head.reverse $ xs) `elemIndex` (reverse.init $ xs)
    where
        go Nothing = 0
        go (Just n) = n + 1

addNext :: [Int] -> Integer -> [Int]
addNext xs _ = xs ++ [getNext xs]

main :: IO ()
main = do
    input <- map (read::String->Int).splitOn ",".head.lines <$> getContents
    print.head.reverse $ foldl addNext input [7..2020]
