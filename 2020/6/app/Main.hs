import Data.List
import Data.List.Split (splitOn)

answered_by_all :: [String] -> [Char]
answered_by_all group = filter (\c -> (all (\line -> c `elem` line) group)) $ head $ group

main :: IO ()
main = do
    input <- readFile "input"
    print.sum.fmap (length.answered_by_all) $ fmap (fmap nub.sort) $ fmap lines $ splitOn "\n\n" input
