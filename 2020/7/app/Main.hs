import Data.List.Split (splitOn)
import Text.Regex.PCRE

data Bag = Bag (String, [(String, Int)]) deriving Show

findBag :: [Bag] -> String -> Bag
findBag bags s = head $ filter (\(Bag (name, _)) -> name == s) bags

countHolding :: [Bag] -> Bag -> Int
countHolding bags (Bag (name, subBags)) =
    sum $ fmap (\(name, count) -> count + (count * countHolding bags (findBag bags name))) subBags

carriesBag :: [Bag] -> Bag -> String -> Bool
carriesBag bags (Bag (name, subBags)) s =
    any (\(name, subbags) -> (name == s || (carriesBag bags (findBag bags name) s))) subBags

-- Takes a string like "2 bright white bags" returning ("bright white", 2)
parseSubBag :: String -> (String, Int)
parseSubBag s =
    let matches = s =~ "^([0-9]+) ([a-z ]+) bag" :: [[String]]
    in (head.reverse.head $ matches, read.head.tail.head $ matches :: Int)

parseContents :: String -> [(String, Int)]
parseContents "no other bags." = []
parseContents s = fmap parseSubBag $ splitOn ", " s

parseBag :: String -> Bag
parseBag s =
    let parts = splitOn " bags contain " s
    in Bag (head parts, parseContents $ head $ tail parts)

main :: IO ()
main = do
    input <- readFile "input"
    let bags = fmap parseBag.lines $ input
    print $ countHolding bags (findBag bags "shiny gold")
