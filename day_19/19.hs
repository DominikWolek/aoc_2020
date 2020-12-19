import Text.Printf
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Data.Either
import Control.Monad (foldM)

inputPath1 = "input"
inputPath2 = "input-1"

type Val = Either [[Int]] Char
type Dict = M.Map Int Val

parseRules :: Dict -> String -> Dict
parseRules dict str = nextDict
    where
        [i, rest] = splitOn ": " str
        calcVals ('"':letter:_) = Right letter
        calcVals rest = Left $ map (map (\x -> read x :: Int).splitOn " ").splitOn " | " $ rest
        vals = calcVals rest
        index = read i
        nextDict = M.insert index vals dict

solution :: Dict -> [String] -> Int
solution dict messeges = length.filter (check [0] dict) $ messeges

check :: [Int] -> Dict -> String -> Bool
check [] _ [] = True
check (index:rest) dict (x:xs) = apply rule
    where
        rule = dict M.! index
        apply (Right letter) = if letter == x then check rest dict xs else False
        apply (Left list) = any (\indexes -> check (indexes ++ rest) dict (x:xs)) list
check _ _ _ = False

main :: IO ()
main = do
    [rulesStr1, messeges1] <- map lines.splitOn "\n\n" <$> readFile inputPath1
    let dict1 = foldl parseRules M.empty rulesStr1
    [rulesStr2, messeges2] <- map lines.splitOn "\n\n" <$> readFile inputPath2
    let dict2 = foldl parseRules M.empty rulesStr2
    printf "Silver star:\t%d\n" $ solution dict1 messeges1
    printf "Gold star:\t%d\n" $ solution dict2 messeges2
