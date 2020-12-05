--- Day 1: Report Repair
import Control.Applicative (liftA3)

combinations :: [Int] -> [(Int, Int, Int)]
combinations lst = liftA3 (,,) lst lst lst

filtr :: [(Int, Int, Int)] -> [(Int, Int, Int)]
filtr = filter (\(x, y, z) -> x + y + z == 2020)

multiply :: [(Int, Int, Int)] -> [Int]
multiply = fmap (\(x, y, z) -> x * y * z)

main :: IO ()
main = do
    f <- readFile "1.txt"
    let ints = fmap (read::String->Int) $ lines $ f
    print.head.multiply.filtr.combinations $ ints
