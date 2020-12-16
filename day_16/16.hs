import Text.Printf
import Data.List (sortBy)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Range = (Int, Int)
type Ticket = [Int]
data Rule = Rule String Range Range deriving (Ord, Eq, Show)

inputPath = "input"

validate :: Rule -> Int -> Bool
validate (Rule _ (r1D, r1U) (r2D, r2U)) x = (r1D <= x && r1U >= x) || (r2D <= x && r2U >= x)

invalid :: [Rule] -> Ticket -> Ticket
invalid rules ticket = foldl doInvalid ticket rules
    where 
        doInvalid curr rule = filter (not.(validate rule)) curr

first :: [Rule] -> [Ticket] -> Int
first rules = sum.map (sum.invalid rules)

getSets :: [Rule] -> [Ticket] -> [S.Set Int]
getSets rules tickets = map getSet rules
    where
        getSet rule = foldl (getPossible rule) (S.fromList [0..19]) tickets 
        getPossible rule possible ticket = toSet possible.map fst.filter (\(x, y) -> validate rule $y).zip [0..] $ ticket
        toSet possible = S.intersection possible.S.fromList

sortSets :: [S.Set Int] -> [(Int, S.Set Int)]
sortSets = sortBy setCompare.zip [0..]
    where
        setCompare (_, a) (_, b) = compare (S.size a) (S.size b)

mergeSets :: [(Int, S.Set Int)] -> M.Map Int Int
mergeSets = snd.foldl (\(setL, dict) (n, setR) ->  (S.union setL setR, M.insert n (S.findMin (S.difference setR setL)) dict)) (S.empty, M.empty)

second :: [Rule] -> [Ticket] -> Ticket -> Int
second rules valid ticket = product $ map (ticket!!) $ departures
    where
        ruleIndexes = mergeSets.sortSets.getSets rules $ valid
        departures = take 6.M.elems $ ruleIndexes


parseTicket :: String -> [Int]
parseTicket = map read.splitOn ","

parseRule rule = Rule field (range1D, range1U) (range2D, range2U)
    where
        [field, values] = splitOn ": " rule
        [[range1D, range1U], [range2D, range2U]] = map (map read.splitOn "-").splitOn " or " $ values

main :: IO ()
main = do
    [rulesS, ticketS, nearbyS] <- splitOn "\n\n" <$> readFile inputPath
    let rules = map parseRule.lines $ rulesS
    let ticket = parseTicket.(!!1).lines $ ticketS
    let nearby = map parseTicket.tail.lines $ nearbyS
    printf "Silver star:\t%d\n" $ first rules nearby
    let valid = filter ((==[]).invalid rules) nearby
    printf "Gold star:  \t%d\n" $ second rules valid ticket
