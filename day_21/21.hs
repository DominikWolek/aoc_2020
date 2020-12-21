import Text.Printf
import Data.List.Split (splitOn)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Data.List
import Data.Function (on)
import qualified Data.Bifunctor as BF (second)

type LineSets = (S.Set String, S.Set String)
type Possible = (String, S.Set String)
type Occurences = M.Map String Int

inputPath = "input"

parse line = (stuffWords, integrientsWords)
    where
        [stuff, integrients] = splitOn " (contains " line
        stuffWords = splitOn " " stuff
        integrientsWords = splitOn ", " $ init integrients

parseLine (f, s) = (S.fromList f, S.fromList s)

occurences :: Occurences -> LineSets -> Occurences
occurences dict (integrients, _) = foldl insert dict integrients
    where
        current key = fromMaybe 0 (M.lookup key dict)
        insert accDict key = M.insert key (current key + 1) accDict

first :: [String] -> [LineSets] -> Int
first noAllergens input = sum.map (occurencesMap M.!) $ noAllergens
    where
        occurencesMap = foldl occurences M.empty input

second :: [Possible] -> String
second possible = intercalate ",".map snd.sortBy (compare `on` fst) $  assignement
    where
        sets = sortBy (compare `on` S.size.snd) possible
        assignement = getAssignemets (M.fromList possible) (head sets)

getNonAlergants :: S.Set String -> [Possible] -> [String]
getNonAlergants integrients = S.toList.foldl S.difference integrients.map snd

getPossible :: [LineSets] -> [String] -> [Possible]
getPossible input = map (\a -> (a, a `possible` input))
    where
        possible allergen = foldl1 S.intersection.map fst.filter (S.member allergen.snd)

getAssignemets :: M.Map String (S.Set String) -> Possible -> [(String, String)]
getAssignemets dict start = doGet [start] dict []
    where
        doGet [] dict result = result
        doGet ((name, singleton):xs) dict result = doGet newStart nextDict newRes
            where
                val = S.findMin singleton
                updated = M.map (S.delete val) dict
                newStartMap = M.filter ((==1).S.size) updated
                newStart = M.toList newStartMap ++ xs
                nextDict = M.difference updated newStartMap
                newRes = (name, val):result

main :: IO ()
main = do
    inpLines <- map parse.lines <$> readFile inputPath
    let input = map parseLine inpLines
    let allergensL = S.toList.S.fromList.concatMap snd $ inpLines
    let possible = getPossible input allergensL
    let integrientsS = S.fromList.concatMap fst $ inpLines
    let noAllergens = getNonAlergants integrientsS possible
    printf "Silver star:\t%d\n" $ first noAllergens input
    printf "Gold star:  \t%s\n" $ second possible
