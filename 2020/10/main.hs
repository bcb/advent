import Data.List

arrangements :: [Int] -> Int
arrangements lst =
    case lst of
        1 : 1 : 1 : 1 : l -> arrangements l * 7
        1 : 1 : 1 : l -> arrangements l * 4
        1 : 1 : l -> arrangements l * 2
        _ : l -> arrangements l
        [] -> 1

main :: IO ()
main = do
    input <- readFile "input"
    let all = sort $ (0:).fmap read.lines $ input
    let differences = zipWith (-) (tail all) all
    print $ (length $ filter (==1) differences) * (length $ filter (==3) differences)
    print $ arrangements differences
