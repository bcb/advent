import Data.List

main :: IO ()
main = do
    input <- readFile "input"
    let all = fmap (read::String->Int).lines $ input
    let allPlusBuiltin = sort $ [0, (maximum all) + 3] ++ all
    let differences = fmap (\(x, y) -> y - x).zip allPlusBuiltin $ tail allPlusBuiltin
    print $ (length $ filter (==1) differences) * (length $ filter (==3) differences)
