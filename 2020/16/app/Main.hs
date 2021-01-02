import Data.List (transpose, isPrefixOf)
import Data.List.Split (splitOn)

numRules = 20
lineMyTicket = 22
numHeader = 25

data Rule = Rule String (Int, Int) (Int, Int) deriving Show

parseRange :: String -> (Int, Int)
parseRange s = let ints = map read.splitOn "-" $ s in (head ints, last ints)

parseRule :: String -> Rule
parseRule s =
    let parts = splitOn ":" s
        w = words $ parts !! 1
    in Rule (parts !! 0) (parseRange (w !! 0)) (parseRange (w !! 2))

validAgainstRule :: Rule -> Int -> Bool
validAgainstRule (Rule _ (min1, max1) (min2, max2)) n =
    ((n >= min1) && (n <= max1)) || ((n >= min2) && (n <= max2))

validAgainstRules :: [Rule] -> Int -> Bool
validAgainstRules rules ticket = any (\rule -> validAgainstRule rule ticket) rules

validTicket :: [Rule] -> [Int] -> Bool
validTicket rules ticket = all (validAgainstRules rules) ticket

findRule :: [Rule] -> [Int] -> Rule
findRule rules field = head.filter (\r -> all (validAgainstRule r) field) $ rules

main :: IO ()
main = do
    input <- lines <$> getContents
    let rules = map parseRule.take numRules $ input
    let tickets = map (map (read :: String -> Int)) $ map (splitOn ",").drop numHeader $ input
    let myTicket = map (read :: String -> Int).splitOn "," $ input !! lineMyTicket
    print.map (findRule rules).transpose.filter (validTicket rules) $ tickets
    -- print.filter (\(_, Rule s _ _) -> isPrefixOf "departure" s).zip myTicket.
