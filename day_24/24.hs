import Text.Printf
import Data.List.Split (splitOn)
import qualified Data.Map.Lazy as M
import Data.Maybe
import Data.Bits (xor)
import Control.Monad (replicateM)

type Cords = (Int, Int)
type Dict = M.Map Cords Int
inputPath = "input"

step :: Cords -> String -> Cords
step curr [] = curr
step (ne, e) ('n':'w':xs) = step (ne + 1, e - 1) xs
step (ne, e) ('n':'e':xs) = step (ne + 1, e) xs
step (ne, e) ('s':'w':xs) = step (ne - 1, e) xs
step (ne, e) ('s':'e':xs) = step (ne - 1, e + 1) xs
step (ne, e) ('w':xs) = step (ne, e - 1) xs
step (ne, e) ('e':xs) = step (ne, e + 1) xs

first :: [String] -> Dict -> Dict
first [] dict = dict
first (x:xs) dict = first xs $ insert dict (step (0, 0) x)
    where
        insert dict x = M.insert x (fromMaybe 0 (M.lookup x dict) `xor` 1) dict

maxBlack :: Dict -> Int
maxBlack = (+1).M.foldlWithKey (\ab (x, y) _ -> maximum [ab, abs x, abs y]) 0

insert :: Dict -> Int -> Dict
insert dict maxB = foldl (doInsert dict) dict $ map (\[a, b] -> (a, b)) $ replicateM 2 [-maxB..maxB]

doInsert :: Dict -> Dict -> Cords -> Dict
doInsert dict curr x
    | black x dict && (bA == 0 || bA > 2) = M.insert x 0 curr
    | white x dict && bA == 2 = M.insert x 1 curr
    | otherwise = curr
        where 
            bA = blackAdjacent x dict

black x dict = maybe False (==1) (M.lookup x dict)
white x dict = maybe True (==0) (M.lookup x dict)

blackAdjacent (x, y) dict = length $ filter (`black` dict) adj
    where
        adj = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1), (x + 1, y - 1), (x - 1, y + 1)]

secondRound :: Dict -> Dict
secondRound old = insert old (maxBlack old)

second :: [String] -> Dict -> Int -> Dict
second input dict n = (!!n) $ iterate secondRound dict

blacks = M.size.M.filter (==1)

main :: IO ()
main = do
    input <- lines <$> readFile inputPath
    let res = first input M.empty
    printf "Silver star:\t%d\n" $ blacks res
    printf "Gold star:  \t%d\n" $ blacks $ second input res 100