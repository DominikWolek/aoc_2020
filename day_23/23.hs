
import Text.Printf
import Data.List.Split (splitOn)
import Data.Maybe
import Data.List
import qualified Data.IntMap.Strict as M

inputPath = "input"

type Cups = M.IntMap Int
type CupsState = (Int, Int, Cups, Int)

calcNum :: Int -> [Int] -> Int -> Int
calcNum x list max
    | x <= 0 = calcNum max list max
    | x `elem` list = calcNum (x - 1) list max
    | otherwise = x

doRound2 :: CupsState -> CupsState
doRound2 (first, last, current, max) = (newFirst, first, newDict, max)
    where
        a = current M.! first
        b = current M.! a
        c = current M.! b
        newFirst = current M.! c
        num = calcNum (first - 1) [a, b, c] max
        numNext = current M.! num
        newDict = M.insert c numNext $ M.insert num a $ M.insert last first $ M.insert first newFirst current

first :: Cups -> String
first input = intercalate "".map show.(\(_, _, x, _) -> after 1 1 x).(!!100).iterate doRound2 $ start
    where
        start = (5, 2, input, 9)

after :: Int -> Int -> Cups -> [Int]
after n stop dict
    | next == stop = []
    | otherwise = next:after next stop dict
    where
        next = dict M.! n

second :: Int -> (Int, Int, Cups, Int) -> Int
second 0 (_, _, dict, _) = product.take 2.after 1 1 $ dict
second n curr = second (n - 1) (doRound2 curr)

parse :: String -> [Int]
parse = map (\x -> read [x])

main :: IO ()
main = do
    input <- parse <$> readFile inputPath
    let inpf = M.fromList $ zip [1..] [4, 5, 8, 7, 3, 2, 6, 9, 1]
    let big = M.insert 1000000 5 $ M.fromList $ zip [10..1000000] [11..]
    let starting = M.union big.M.insert 2 10 $ inpf
    printf "Silver star:\t%s\n" $ first inpf
    printf "Gold star:  \t%d\n" $ second (10000000 :: Int) (5, 1000000 :: Int, big)
