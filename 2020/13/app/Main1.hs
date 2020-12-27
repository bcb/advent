module Main1 where

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.List.Split (splitOn)

parseIds :: String -> [Int]
parseIds = fmap read.filter (/="x").(splitOn ",")

nextBus :: Int -> Int -> Int -> Int
nextBus myArrival maxBusIds busId = head.filter (\t -> t `mod` busId == 0) $ [myArrival..myArrival+maxBusIds]

main :: IO ()
main = do
    input <- lines <$> readFile "input"
    let (myArrival, busIds) = (read.head $ input, parseIds.head.tail $ input)
    let (busId, busArrival) = head.sortBy (comparing snd).fmap (\b -> (b, nextBus myArrival (maximum busIds) b)) $ busIds
    print $ busId * (busArrival - myArrival)
