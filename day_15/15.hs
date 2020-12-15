import Text.Printf
import Data.List.Split (splitOn)
import Data.List (elemIndex)
import qualified Data.Map as M

inputPath = "input"

first :: [Int] -> Int -> Int
first (x:xs) 0 = x
first (x:xs) n = first (index (elemIndex x xs):x:xs) (n - 1)
    where
        index Nothing = 0
        index (Just y) = y + 1

second :: M.Map Int Int -> Int -> Int -> Int
second _ 29999999 prev = prev
second dict n prev = second nextDict (n + 1) (index (M.lookup prev dict))
    where
        index Nothing = 0
        index (Just y) = n - y
        nextDict = M.insert prev n dict

main :: IO ()
main = do
    input <- map (\x -> read x::Int).splitOn "," <$> readFile inputPath
    let dict = M.fromList $ zip (init input) [0..]
    printf "Silver star:\t%d\n" $ first (reverse input) 2013
    printf "Gold star:  \t%d\n" $ second dict 6 (last input) 