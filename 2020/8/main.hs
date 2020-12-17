data Op = Nop | Acc Int | Jmp Int deriving Show
type Program = [Op]
type AccVal = Int
type LineNumber = Int
type Executed = [Int]

run :: Program -> AccVal -> LineNumber -> Executed -> AccVal
run program accVal lineNumber executed
    | lineNumber `elem` executed = accVal
    | lineNumber >= length program = accVal
    | otherwise =
        case (program !! lineNumber) of
            Nop -> run program accVal (lineNumber + 1) (executed ++ [lineNumber])
            Acc num -> run program (accVal + num) (lineNumber + 1) (executed ++ [lineNumber])
            Jmp num -> run program accVal (lineNumber + num) (executed ++ [lineNumber])

parse :: String -> Op
parse s =
    let parts = words s
        (op, num) = (head $ parts, (read (dropWhile (=='+').head.tail $ parts)::Int))
    in
    case op of
        "nop" -> Nop
        "acc" -> Acc num
        "jmp" -> Jmp num

main :: IO ()
main = do
    input <- readFile "input"
    let program = fmap parse.lines $ input
    print $ run program 0 0 []
