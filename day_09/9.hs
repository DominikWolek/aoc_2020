import Text.Printf
import Data.List.Split (splitOn)
import qualified Data.Set as S

inputPath = "input"

first :: [Int] -> [Int] -> Int
first (y:ys) (x:xs) = 
    if check (S.fromList (y:ys)) x
        then x
        else first (ys ++ [x]) xs

check :: S.Set Int -> Int -> Bool
check set x
  | S.size set < 2 = True
  | (x - m /= m) && S.member (x - m) newSet = False
  | otherwise = check newSet x
  where
      (m, newSet) = S.deleteFindMin set

second :: [Int] -> Int -> Int
second (x:xs) n
    | weakness /= 0 = weakness
    | otherwise = second xs n
    where
        weakness = findWeakness xs n x [x]

findWeakness :: [Int] -> Int -> Int -> [Int] -> Int
findWeakness (x:xs) n curr list
    | curr + x > n = 0
    | curr + x == n = sum [minimum (x:list), maximum (x:list)]
    | otherwise = findWeakness xs n (x + curr) (x:list)

main :: IO ()
main = do
    input <- map read.lines <$> readFile inputPath
    let ans = first (take 25 input) (drop 25 input)
    printf "Silver star:\t%d\n" ans
    printf "Gold star:  \t%d\n" $ second input ans
