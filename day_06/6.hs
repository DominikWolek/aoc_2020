import Text.Printf
import Data.List.Split (splitOn)
import Data.Set (fromList, size, intersection)

inputPath = "input"

first :: [String] -> Int
first = sum.map (size.fromList)

second :: [[String]] -> Int
second = sum.map (size.allAnswered.map fromList)
    where
        full = fromList ['a'..'z']
        allAnswered = foldl intersection full

main :: IO ()
main = do
    input <- map (splitOn "\n").splitOn "\n\n" <$> readFile inputPath
    printf "Silver star:\t%d\n" $ first (map concat input)
    printf "Gold star:  \t%d\n" $ second input
