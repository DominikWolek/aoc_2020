import Text.Printf
import Data.List.Split (splitOn)

inputPath = "input"

first :: Int -> [Int] -> Int
first start ids = (time - start) * bus
    where
        helper list m  = filter ((==0).(m `mod`)) list
        time = head $ dropWhile (null.(helper ids)) $ iterate (+1) start
        bus = head $ helper ids time

euclid :: Int -> Int -> (Int, Int)
euclid a b = doEuclid a b 1 0 0 1

doEuclid :: Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int)
doEuclid _ 0 x y r s = (x, y)
doEuclid a b x y r s = doEuclid b c r s r' s'
    where 
        c = a `mod` b
        q = a `div` b
        r' = x - (q * r)
        s' = y - (q * s)
 
firstPositive :: Int -> Int -> Int
firstPositive m = (+m).last.takeWhile (<0).iterate (+m)

second :: [(Int, Int)] -> Int
second modulos = firstPositive m $ sum $ map calcE modulos
    where
        m = product $ map snd modulos
        calcE (y, n) = e * y
            where 
                mi = m `div` n
                (f, g) = euclid n mi
                e = g * mi
  
parse1 :: String -> [Int]
parse1 = map read.filter (/="x").splitOn ","

parse2 :: String -> [(Int, Int)]
parse2 = map (\(i, x) -> ((read x) - i, read x)).filter (\(_, x) -> x /= "x").zip [0..].splitOn ","

main :: IO ()
main = do
    [start, ids] <- lines <$> readFile inputPath
    let input1 = parse1 ids
    let input2 = parse2 $ ids
    printf "Silver star:\t%d\n" $ first (read start) input1
    printf "Gold star:  \t%d\n" $ second input2
