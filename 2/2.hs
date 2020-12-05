--- Day 2: Password Philosophy
data Task = Task {
    low :: Int,
    high :: Int,
    char :: Char,
    password :: String
} deriving (Show)

valid :: Task -> Bool
valid task =
    let lowchar = password task !! (low task - 1)
        highchar = password task !! (high task - 1)
    in sum (fmap fromEnum [lowchar == char task, highchar == char task]) == 1

parseLine :: String -> Task
parseLine s =
    let parts = words s
        (l, h) = break (== '-') (parts !! 0)
        c = head $ fst (break (==':') (parts !! 1))
    in Task (read l) (read $ tail h) c (parts !! 2)

main :: IO ()
main = do
    input <- readFile "input"
    print.length.filter valid.fmap parseLine.lines $ input
