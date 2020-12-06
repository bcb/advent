--- Day 3: Toboggan Trajectory
getchar :: (Int, String) -> Char
getchar (position, line) =
    let p = position `mod` (length line)
    in line !! p

main :: IO ()
main = do
    input <- readFile "input"
    print.length.filter (== '#').fmap getchar.zip [3,6..].tail.lines $ input
