import Text.Printf
import qualified Data.Map.Strict as M

type MemoMap = M.Map (Int, Int) Int 

inputPath = "input"

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (p:xs) = quicksort (filter (< p) xs) ++ [p] ++ quicksort (filter (>= p) xs)

differences :: [Int] -> Int -> Int
differences input diff = foldl diffEquals 0 zipped
    where
        diffEquals acc (x, y) = if y - x == diff then acc + 1 else acc 
        zipped = zip input (drop 1 input)

first :: [Int] -> Int
first input = differences input 3 * differences input 1

second :: [Int] -> MemoMap -> (Int, MemoMap)
second (x:[y]) dict = (1, dict)
second (x:y:z:xs) dict
    | M.member (x,y) dict = (dict M.! (x,y), dict)
    | z - x > 3 = (res1, dict1Updated)
    | otherwise = (res1 + res2, dict2Updated)
    where 
        (res1, dict1) = second (y:z:xs) dict
        dict1Updated = M.insert (y,z) res1 dict1
        (res2, dict2) = second (x:z:xs) dict1Updated
        dict2Updated = M.insert (x,y) (res1 + res2) $ M.insert (x,z) res2 dict2

parse :: String -> Int
parse = read

main :: IO ()
main = do
    input <- map parse.lines <$> readFile inputPath
    let additional = maximum input + 3
    let sorted = quicksort (0:additional:input)
    printf "Silver star:\t%d\n" $ first sorted
    printf "Gold star:  \t%d\n" $ fst $ second sorted M.empty
