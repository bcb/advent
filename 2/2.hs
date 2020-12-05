--- Day 2: Password Philosophy
data Task = Task {
    low :: Int,
    high :: Int,
    char :: Char,
    password :: String
} deriving (Show)

ischar :: Int -> String -> Char -> Bool
ischar position pwd c = (pwd !! (position - 1)) == c

valid :: Task -> Bool
valid task =
    sum (fmap fromEnum [
        ischar (low task) (password task) (char task),
        ischar (high task) (password task) (char task)
    ]) == 1

filtr :: [Task] -> [Task]
filtr = filter (\t -> valid t)

parseLine :: String -> Task
parseLine s =
    let parts = words s
        (l, h) = break (== '-') (parts !! 0)
        c = head $ fst (break (==':') (parts !! 1))
    in Task (read l) (read $ tail h) c (parts !! 2)

tasks :: [String] -> [Task]
tasks = fmap (\s -> parseLine s)

main :: IO ()
main = do
    input <- readFile "2.txt"
    print.length.filtr.tasks.lines $ input
