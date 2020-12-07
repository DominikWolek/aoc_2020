import Text.Printf
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Color = (String, String)
type TopoDict = M.Map Color [Color]
type Dict = M.Map Color [(Color, Int)]

inputPath = "input"

gold :: Color
gold = ("shiny", "gold")

parse :: String -> (Color, [(Color, Int)])
parse line = parseBag.words.take (len - 1) $ line
    where 
        len = length line
        parseBag (c1:(c2:("bags":("contain":("no":_))))) = ((c1,c2), [])
        parseBag (c1:(c2:("bags":("contain":insides)))) = ((c1,c2), map getColor.splitOn ",".unwords $ insides)
        getColor = matchBag.words
        matchBag (n:(c1:(c2:_))) = ((c1,c2), read n::Int)

getMaybeList :: Maybe [a] -> [a]
getMaybeList Nothing = []
getMaybeList (Just x) = x

topoDict :: [(Color, [(Color, Int)])] -> TopoDict -> TopoDict
topoDict [] dict = dict
topoDict ((c, insideBags):xs) dict = topoDict xs (insertInside c (map fst insideBags) dict)

insertInside :: Color -> [Color] -> TopoDict -> TopoDict
insertInside c [] dict = dict
insertInside c (x:xs) dict = insertInside c xs newDict
    where
        currentList = getMaybeList $ M.lookup x dict
        newDict = M.insert x (c:currentList) dict

first ::  Color -> TopoDict -> S.Set Color -> S.Set Color
first c dict set = traverse possibleOuterBags newSet
    where
        possibleOuterBags = getMaybeList $ M.lookup c dict
        newSet = S.union (S.fromList possibleOuterBags) set
        traverse [] s = s
        traverse (x:xs) s = traverse xs (first x dict s)

second :: Dict -> Color -> Int
second dict c1 = sum $ map calcInsideBags bags
    where
        bags = dict M.! c1
        calcInsideBags (c2, n) = n + n * second dict c2

main :: IO ()
main = do
    input <- map parse.lines <$> readFile inputPath
    printf "Silver star:\t%d\n" $ S.size.first gold (topoDict input M.empty) $ S.empty
    printf "Gold star:  \t%d\n" $ second (M.fromList input) gold
