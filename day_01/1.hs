import Data.Set (member, fromList)
import Text.Printf
inputPath = "input"    
match = (2020 -)

first :: [Int] -> Int
first input = head [x * match x | x <- input, member (match x) set]
    where
        set = fromList input

second :: [Int] -> Int
second input = head [x * y * match (x + y) | x <- input, y <- input, member (match (x + y)) set]
    where
        set = fromList input

main :: IO ()
main = do
    input <- map read.lines <$> readFile inputPath
    printf "Silver star:\t%d\n" $ first input
    printf "Gold star:\t%d\n" $ second input
