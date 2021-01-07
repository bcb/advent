import Data.List (transpose, isPrefixOf, sortBy)
import Data.Ord (comparing)
import Data.List.Split (splitOn)

numRules = 20
lineOfMyTicket = 22 -- Zero based
numHeader = 25

data Rule = Rule String (Int, Int) (Int, Int) deriving (Show, Eq)

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

countMatches :: Rule -> [Int] -> Int
countMatches rule field = length.filter (==True).map (validAgainstRule rule) $ field

getMatches :: [Rule] -> [Int] -> ([Int], [(Rule, Int)])
getMatches rules field =
    (field, sortBy (flip $ comparing snd) $ map (\r -> (r, countMatches r field)) rules)

isClearWinner :: [(Rule, Int)] -> Bool
isClearWinner (x:[]) = True
isClearWinner matches = (snd.head $ matches) /= (snd.head.tail $ matches)

findClearWinner :: [([Int], [(Rule, Int)])] -> ([Int], (Rule, Int))
findClearWinner ((field, matches):xs)
    | isClearWinner matches = (field, head matches)
    | otherwise = findClearWinner xs

removeItem :: (a -> Bool) -> [a] -> [a]
removeItem _ [] = []
removeItem f (x:xs)
    | f x = removeItem f xs
    | otherwise = x : removeItem f xs

findClearWinners :: [Rule] -> [[Int]] -> [([Int], (Rule, Int))]
findClearWinners _ [] = []
findClearWinners rules fields =
    let winner = findClearWinner.map (getMatches rules) $ fields
    in winner : findClearWinners
        (removeItem (== (fst.snd $ winner)) rules)
        (removeItem (== (fst winner)) fields)

main :: IO ()
main = do
    input <- lines <$> getContents
    let rules = map parseRule.take numRules $ input
    let myTicket = map (read :: String -> Int).splitOn "," $ input !! lineOfMyTicket
    let tickets = map (map (read :: String -> Int)).map (splitOn ",").drop numHeader $ input
    let fields = findClearWinners rules.transpose.(:) myTicket.filter (validTicket rules) $ tickets
    let departureResults = filter (\(_, (Rule name _ _, _)) -> isPrefixOf "departure" name) $ fields
    print.product.head.transpose.map fst $ departureResults
