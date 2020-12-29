import Data.Map (Map, fromList, findWithDefault, insert, (!), size)
import Data.Tuple (swap)
import Data.List.Split (splitOn)

addNext :: (Map Int Int, Int) -> Int -> (Map Int Int, Int)
addNext (m, last) n =
    let distance = n - (findWithDefault n last m)
    in (insert last n m, distance)

main :: IO ()
main = do
    input <- fromList.map swap.zip [0..].map (read::String->Int).splitOn ",".head.lines <$> getContents
    print $ foldl addNext (input, 0) [(size input)..29999998]
