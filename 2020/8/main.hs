data Op = Nop | Acc Int | Jmp Int deriving (Show, Eq)
type Program = [Op]
type AccVal = Int
type LineNumber = Int
type Executed = [Int]

run :: AccVal -> LineNumber -> Executed -> Program -> AccVal
run accVal lineNumber executed program
    | lineNumber `elem` executed = 0
    | lineNumber >= length program = accVal
    | otherwise =
        case (program !! lineNumber) of
            Nop -> run accVal (lineNumber + 1) (executed ++ [lineNumber]) program
            Acc num -> run (accVal + num) (lineNumber + 1) (executed ++ [lineNumber]) program
            Jmp num -> run accVal (lineNumber + num) (executed ++ [lineNumber]) program

parse :: String -> Op
parse s =
    let parts = words s
        (op, num) = (head $ parts, (read (dropWhile (=='+').head.tail $ parts)::Int))
    in
    case op of
        "nop" -> Nop
        "acc" -> Acc num
        "jmp" -> Jmp num

replace :: Program -> LineNumber -> Op -> Program
replace program lineNumber op =
    take lineNumber program ++ [op] ++ drop (lineNumber + 1) program

modifyProgram :: Program -> LineNumber -> Program
modifyProgram program lineNumber =
    case (program !! lineNumber) of
        Acc num -> replace program lineNumber Nop
        Jmp num -> replace program lineNumber Nop

main :: IO ()
main = do
    input <- readFile "input"
    let program = fmap parse.lines $ input
    let accjmps = filter (\(_, op) -> op /= Nop) (zip [0..] program)
    print.fmap (run 0 0 []) $ fmap (\(lineNumber, _) -> modifyProgram program lineNumber) accjmps
