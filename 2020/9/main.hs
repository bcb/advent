import Control.Applicative
import Data.List

preambleLength = 25

preamble :: Int -> [Int] -> [Int]
preamble curr all = take preambleLength $ drop (curr - preambleLength) all

combinations :: [Int] -> [(Int, Int)]
combinations all = (,) <$> all <*> all

valids :: Int -> [Int] -> [Int]
valids curr = fmap (\(x,y) -> x + y).combinations.preamble curr

isValid :: [Int] -> Int -> Bool
isValid all curr = elem (all !! curr) $ valids curr all

contiguousSets :: [a] -> [[a]]
contiguousSets = filter (\s -> length s >= 2).concat.fmap inits.tails

main :: IO ()
main = do
    input <- readFile "input"
    let all = fmap (read :: String -> Int).lines $ input
    let solve1 = head.filter (not.isValid all) $ [preambleLength..(length all)-1]
    let solve2 = head.filter (\s -> (==) (all !! solve1) (sum s)).contiguousSets $ take solve1 all
    print $ minimum solve2 + maximum solve2
