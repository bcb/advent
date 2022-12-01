app :: String -> (int, int)
app s =
    let parts = splitOn "," $ s
    case "forward"

main :: IO ()
main = do
    f <- readFile "input"
    print.foldr app.lines $ f
