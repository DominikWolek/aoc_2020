import Text.Printf
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (transpose)
import Data.Maybe

type Picture = (Int, [String])
type BorderMap = M.Map String (S.Set (Int, Int))

inputPath = "input"

getMatching bordersMap = map (\(n, x) -> (n, map (S.size.(bordersMap M.!)).pictureBorders $ (n, x))) 
getTopLeft = fst.head.dropWhile ((/= [1,2,1,2]).take 4.snd)
getStarting = map fst.filter ((==4).length.filter (==1).snd)
flipP picture = map reverse picture
rotR = rotL.rotL.rotL
rotL = reverse.transpose

upBorder picture = head picture
rightBorder picture = map last picture
downBorder picture = reverse (last picture)
leftBorder picture = reverse(map head picture)

ops :: [[String] -> [String]]
ops = [id, rotL.rotL, rotR, rotL] ++ map (flipP.) [id, rotL.rotL, rotR, rotL]

pictureBorders :: Picture -> [String]
pictureBorders (_, picture) = borders ++ map reverse borders
    where
        borders = [upBorder picture, downBorder picture, leftBorder picture, rightBorder picture]

parse :: [String] -> Picture
parse (title:rest) = (read (init(drop 5 title)), rest)

getBordersMap :: BorderMap -> Picture -> BorderMap
getBordersMap dict (n, picture) = foldl insertB dict borders
    where
        borders = zip (pictureBorders (n, picture)) $ zip (repeat n) [0..] 

insertB :: BorderMap -> (String, (Int, Int)) -> BorderMap
insertB dict (b, val) = M.insert b inserted dict  
    where
        set = fromMaybe S.empty (M.lookup b dict)
        inserted = S.insert val set

getMapLine pictures borders (n, val) borderFunc valFunc = result list
    where
        down = borderFunc val
        list = S.toList $ borders M.! down
        result [(n', _)] = [(n, val)]
        result [(n', fun), (next, nextFun)]
            | n' == n = (n, val) : rest
            | otherwise = result [(next, nextFun), (n', fun)]
            where
                nextVal = valFunc.(ops !! nextFun) $ (pictures M.! next)
                rest = getMapLine pictures borders (next, nextVal) borderFunc valFunc

monster = ["                  # ","#    ##    ##    ###", " #  #  #  #  #  #   "]
monserLen = length.head $ monster

join = concatMap joinLine

joinLine = map concat.transpose.map (map (init.tail).init.tail)

getImage topLeft bordersMap picturesMap = join $ map (map snd) raw
    where
        topLeftPicture = (topLeft, picturesMap M.! topLeft) 
        left = getMapLine picturesMap bordersMap topLeftPicture downBorder flipP
        raw = map (\x -> getMapLine picturesMap bordersMap x rightBorder (flipP.rotR)) left

second image n = result
    where
        currentImage = (ops !! n) image 
        monsterNum = countMonsters currentImage
        total = length.filter(=='#').concat $ currentImage
        monsterSize = length.filter(=='#').concat $ monster
        value = total - monsterSize * monsterNum
        result = if monsterNum > 0 then value else second image (n + 1) 

countMonsters (x:y:z:rest) = val + countMonsters (y:z:rest)
    where
        val = doCountMonsters [x,y,z]
        doCountMonsters [x,y,z]
            | length x >= monserLen = 
                if all match $ zip [x,y,z] monster
                    then 1 + doCountMonsters [tail x, tail y, tail z]
                    else doCountMonsters [tail x, tail y, tail z]
            | otherwise = 0
countMonsters _ = 0

match (line1, line2) = all doMatch (zip line1 line2)
    where
        doMatch (_,' ') = True
        doMatch ('#','#') = True
        doMatch (_,_) = False

main :: IO ()
main = do
    input <- map (parse.lines).splitOn "\n\n" <$> readFile inputPath
    let picturesMap = M.fromList input
    let bordersMap = foldl getBordersMap M.empty input
    let matchingBorders = getMatching bordersMap input
    let topLeft = getTopLeft matchingBorders
    let image = getImage topLeft bordersMap picturesMap
    -- print $ numMatchingBorders input bordersMap
    -- let image = getImage starting bordersMap picturesMap
    printf "Silver star:\t%d\n" $ product.getStarting $ matchingBorders
    printf "Gold star:\t%d\n" $ second image 0
