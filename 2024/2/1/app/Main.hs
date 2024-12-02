import Data.List (sort)
import Data.List.Split (splitOn)

diffs :: [Int] -> [Int]
diffs lst = map (\(x, y) -> x - y) $ zip lst (tail lst)

isSafe :: [Int] -> Bool
isSafe lst = (all (> 0) lst || all (< 0) lst) && all (\x -> (abs x) < 4) lst

main :: IO ()
main = do
  inp <- readFile "input"
  let ns = map (map (\x -> read x :: Int) . splitOn " ") . lines $ inp
  print . length . filter isSafe . map diffs $ ns
