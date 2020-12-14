import Text.Printf
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Bits

data Line = Mem Int Int | Mask String 
    deriving Show

inputPath = "input"

binToDec :: [Int] -> Int
binToDec = foldl (\acc x -> 2 * acc + x) 0

decToBin :: Int -> [Int]
decToBin x = res
    where
        doBin 0 = []
        doBin y = (y `mod` 2):(doBin (y `div` 2))
        res = reverse $ doBin x

leftPad :: Int -> a -> [a] -> [a]
leftPad n x val = replicate (n - (length val)) x ++ val

parse :: String -> Line
parse ('m':'a':xs) = Mask ((!!1).splitOn " = " $ xs)
parse ('m':'e':'m':'[':xs) = Mem x y
    where
        line = splitOn " = " xs
        y = read (line !! 1)
        x = read (init.head $ line)  

getMaskNum :: Int -> Int -> Int -> String -> Int
getMaskNum z o x = binToDec.map change
    where 
        change '0' = z
        change '1' = o
        change 'X' = x

getFirstMask :: String -> (Int -> Int)
getFirstMask x = (\y -> (getMaskNum 0 1 0 x) .|. (getMaskNum 0 1 1 x) .&. y)

first :: (Int -> Int) -> (M.Map Int Int) -> [Line] -> Int
first _ dict [] = sum $ M.elems dict
first _ dict ((Mask x):xs) = first (getFirstMask x) dict xs
first mask dict ((Mem i val):xs) = first mask (M.insert i (mask val) dict) xs

subset :: String -> [Int]
subset mask = getSubset (nextEl 0)
    where
        xMask = getMaskNum 0 0 1 mask
        nextEl num = (num + 1 + (complement xMask)) .&. xMask
        getSubset 0 = [0]
        getSubset num = num:(getSubset (nextEl num))

toMask :: Int -> String
toMask = map (head.show).decToBin

getSecondMask :: String -> (Int -> [Int])
getSecondMask m = (\x -> map (xor (zMask x)) (subset x)).merge 
    where
        zMask = getMaskNum 0 1 0
        merge = map doMerge.zip m.leftPad (length m) '0'.toMask
        doMerge ('X', _) = 'X'
        doMerge ('1', _) = '1'
        doMerge (_, x) = x

second :: (Int -> [Int]) -> (M.Map Int Int) -> [Line] -> Int
second _ dict [] = sum $ M.elems dict
second _ dict ((Mask x):xs) = second (getSecondMask x) dict xs
second mask dict ((Mem i val):xs) = second mask nextDict xs
    where
        nextDict = foldl (\acc x -> M.insert x val acc) dict (mask i)

main :: IO ()
main = do
    input <- map parse.lines <$> readFile inputPath
    printf "Silver star:\t%d\n" $ first id M.empty input
    printf "Gold star:  \t%d\n" $ second (replicate 0) M.empty input
