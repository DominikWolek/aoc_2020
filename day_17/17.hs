import Text.Printf
import Data.List.Split (splitOn)
import Control.Monad (replicateM)
import qualified Data.Map as M

type Cords = (Int, Int, Int)
type CordMap = M.Map Cords Int

type Cords4 = (Int, Int, Int, Int)
type CordMap4 = M.Map Cords4 Int
    
inputPath = "input"

parse :: Int -> Int -> CordMap -> [String] -> CordMap
parse x y dict [] = dict
parse x y dict ([]:rest) = parse 0 (y + 1) dict rest
parse x y dict ((a:as):bs)
    | a == '#' = parse (x + 1) y (M.insert (x, y, 0) 1 dict) (as:bs)
    | otherwise = parse (x + 1) y (M.insert (x, y, 0) 0 dict) (as:bs)

parse4 :: Int -> Int -> CordMap4 -> [String] -> CordMap4
parse4 x y dict [] = dict
parse4 x y dict ([]:rest) = parse4 0 (y + 1) dict rest
parse4 x y dict ((a:as):bs)
    | a == '#' = parse4 (x + 1) y (M.insert (x, y, 0, 0) 1 dict) (as:bs)
    | otherwise = parse4 (x + 1) y (M.insert (x, y, 0, 0) 0 dict) (as:bs)

getStarting :: CordMap
getStarting = M.fromList $ map (\[x, y, z] -> ((x, y, z), 0)) $ replicateM 3 [-7..15]

getStarting4 :: CordMap4
getStarting4 = M.fromList $ map (\[x, y, z, w] -> ((x, y, z, w), 0)) $ replicateM 4 [-7..15]

neighbours (x, y, z) = map (\[xp, yp, zp] -> (x + xp, y + yp, z + zp)) dropZero
    where
        nums = replicateM 3 [0, 1, -1]
        dropZero = (takeWhile (/= [0,0,0]) nums) ++ (tail (dropWhile (/= [0,0,0]) nums))

neighbours4 (x, y, z, w) = map (\[xp, yp, zp, wp] -> (x + xp, y + yp, z + zp, w + wp)) dropZero
    where
        nums = replicateM 4 [0, 1, -1]
        dropZero = (takeWhile (/= [0,0,0,0]) nums) ++ (tail (dropWhile (/= [0,0,0,0]) nums))

doRound :: CordMap -> CordMap -> Cords -> CordMap
doRound prev curr cords
    | state == 0 && neighbourSum == 3 = M.insert cords 1 curr
    | state == 1 && (neighbourSum == 2 || neighbourSum == 3) = M.insert cords 1 curr
    | otherwise = M.insert cords 0 curr
    where
        state = prev M.! cords
        neighbourSum = sum $ map (prev M.!) (neighbours cords)

oneRound :: CordMap -> CordMap
oneRound dict = foldl (doRound dict) getStarting (map (\[x, y, z] -> (x, y, z)) $ replicateM 3 [-6..14])

doRound4 :: CordMap4 -> CordMap4 -> Cords4 -> CordMap4
doRound4 prev curr cords
    | state == 0 && neighbourSum == 3 = M.insert cords 1 curr
    | state == 1 && (neighbourSum == 2 || neighbourSum == 3) = M.insert cords 1 curr
    | otherwise = M.insert cords 0 curr
    where
        state = prev M.! cords
        neighbourSum = sum $ map (prev M.!) (neighbours4 cords)

oneRound4 :: CordMap4 -> CordMap4
oneRound4 dict = foldl (doRound4 dict) getStarting4 (map (\[x, y, z, w] -> (x, y, z, w)) $ replicateM 4 [-6..14])

solution dict f = sum $ M.elems end
    where
        end = (!!6) $ iterate f dict

main :: IO ()
main = do
    f <- lines <$> readFile inputPath
    let input = parse 0 0 getStarting f
    printf "Silver star:\t%d\n" $ solution input oneRound
    let input4 = parse4 0 0 getStarting4 f
    printf "Gold star:  \t%d\n" $ solution input4 oneRound4
