import Data.List.Split (splitOn)
import Data.Map (Map, findWithDefault, fromList, insert, size)
import Data.Tuple (swap)

addNext :: (Map Int Int, Int) -> Int -> (Map Int Int, Int)
addNext (m, last) n = (insert last n m, n - (findWithDefault n last m))

main :: IO ()
main = do
  input <- fromList . map swap . zip [0 ..] . map (read :: String -> Int) . splitOn "," . head . lines <$> getContents
  print $ foldl addNext (input, 0) [(size input) .. 29999998]
