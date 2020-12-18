import Control.Applicative

preambleLength = 5

preamble :: Int -> [Int] -> [Int]
preamble curr all = take preambleLength $ drop (curr - preambleLength) all

combinations :: [Int] -> [(Int, Int)]
combinations all = (,) <$> all <*> all

valids :: Int -> [Int] -> [Int]
valids curr = fmap (\(x,y) -> x + y).combinations.preamble curr

isValid :: [Int] -> Int -> Bool
isValid all curr = elem (all !! curr) $ valids curr all

solve1 :: [Int] -> Int
solve1 all = (all !!).head.filter (not.isValid all) $ [preambleLength..(length all)-1]

solve2 :: [Int] -> Int
solve2 all = (all !!).head.filter (not.isValid all) $ [preambleLength..(length all)-1]

main :: IO ()
main = do
    input <- readFile "input"
    let all = fmap (read :: String -> Int).lines $ input
    print.solve1 $ all
