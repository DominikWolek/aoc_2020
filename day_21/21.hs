import Text.Printf
import Data.List.Split (splitOn)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Data.List

type LineSets = (S.Set String, S.Set String)
type Assignement = M.Map String String
type Occurences = M.Map String Int

inputPath = "input"

parse line = (stuffWords, integrientsWords)
    where
        [stuff, integrients] = splitOn " (contains " line
        stuffWords = splitOn " " stuff
        integrientsWords = splitOn ", " $ init integrients

replace :: [LineSets] -> String -> String -> Maybe [LineSets]
replace input a i = doReplace input []
    where
        doReplace [] list = Just list
        doReplace ((ints, allers):xs) list = 
            if (S.member a allers) && not (S.member i ints)
                then Nothing
                else doReplace xs ((S.delete i ints, S.delete a allers):list)
    

getAssignement :: Assignement -> [String] -> [String] -> [String] -> [LineSets] -> Int -> Maybe (Assignement)
getAssignement dict _ _ rest _ 0 = Just dict
getAssignement dict _ [] _ _ _  = Nothing
getAssignement dict (a:as) (i:is) rest input n = if isNothing left then right else left
    where
        newDict = M.insert a i dict
        newLineSets = replace input a i
        left = maybe Nothing 
                     (\inp -> getAssignement newDict (as) (is++rest) rest inp (n - 1)) 
                     newLineSets
        right = getAssignement dict (a:as) (is) (i:rest) input n

occurences :: Occurences -> LineSets -> Occurences
occurences dict (integrients, _) = foldl (\acc key -> M.insert key (current key + 1) acc) dict integrients
    where
        current key = fromMaybe 0 (M.lookup key dict)

first :: [String] -> [LineSets] -> Int
first noAllergens input = sum.map (occurrsMap M.!) $ noAllergens
    where
        occurrsMap = foldl occurences M.empty input

second :: Assignement -> String
second = intercalate ",".M.elems

main :: IO ()
main = do
    inpLines <- map parse.lines <$> readFile inputPath
    let input = map (\(f, s) -> (S.fromList f, S.fromList s)) inpLines
    let integrientsS = S.fromList.concat.map fst $ inpLines
    let allergensS = S.fromList.concat.map snd $ inpLines
    let integrientsL = S.toList integrientsS
    let allergensL = S.toList allergensS
    let assignement = fromJust $ getAssignement M.empty allergensL integrientsL [] input (S.size allergensS)
    let noAllergens =  S.toList.S.difference integrientsS.S.fromList.M.elems $ assignement
    printf "Silver star:\t%d\n" $ first noAllergens input
    printf "Gold star:  \t%s\n" $ second assignement
