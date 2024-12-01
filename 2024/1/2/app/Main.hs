import Data.List.Split (splitOn)

occurrences :: Eq a => a -> [a] -> Int
occurrences x = length . filter (x ==)

main :: IO ()
main = do
  lst <- readFile "input"
  let splitted = map (splitOn "   ") . lines $ lst
      fsts = map head $ splitted
      snds = map (!! 1) $ splitted
  print . sum . map (\x -> (read x :: Int) * occurrences x snds) $ fsts
