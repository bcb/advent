col :: Int -> Int -> String -> Int
col lower upper [] = lower
col lower upper (x:xs)
    | x `elem` "FL" = col lower (lower + (upper - lower) `div` 2) xs
    | x `elem` "BR" = col (lower + (upper - lower) `div` 2 + 1) upper xs

-- Takes a boarding pass like "BFFFBBFRRR" and returns the seatId like 567.
seatId :: String -> Int
seatId xs =
    let parts = (take 7 xs, drop 7 xs)
    in
    (col 0 127 (fst parts)) * 8 + col 0 7 (snd parts)

main :: IO ()
main = do
    input <- readFile "input"
    print $ fmap seatId $ lines input
    print $ filter (not. (`elem` results)) [seatId "FFFFFFFLLL"..seatId "BBBBBBBRRR"]
