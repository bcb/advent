--- Day 3: Toboggan Trajectory

getchar :: (String, Int) -> Char
getchar (line, position) =
    let p = position `mod` (length line)
    in line !! p

run :: [String] -> [Int] -> Int
run slope positions = length.filter (== '#').fmap getchar $ zip slope positions

everysecond (x:y:xs) = y:everysecond xs;
everysecond _ = []

main :: IO ()
main = do
    input <- readFile "input"
    let slope = tail.lines $ input
    print $ product $ fmap (run $ slope) [[1..],[3,6..],[5,10..],[7,14..]] ++ [run (everysecond $ slope) [1..]]
