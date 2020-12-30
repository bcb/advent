import Data.List.Split (splitOn)

data Rule = Rule String ((Int, Int), (Int, Int)) deriving Show

parseRange :: String -> (Int, Int)
parseRange s = let ints = map read.splitOn "-" $ s in (head ints, last ints)

parseRule :: String -> Rule
parseRule s = let w = words s in Rule (w !! 0) (parseRange (w !! 1), parseRange (w !! 3))

validNumber :: [Rule] -> Int -> Bool
validNumber = undefined

main :: IO ()
main = do
    input <- lines <$> getContents
    let rules = map parseRule.take 3 $ input
    let tickets = map (map (read :: String -> Int)) $ map (splitOn ",").drop 8 $ input
    print tickets
    -- print.map (filter (not.validNumber rules)) $ tickets
