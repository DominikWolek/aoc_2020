import Text.Printf
import Data.Set (member, fromList)

inputPath = "input"

first :: [Int] -> Int
first = maximum

calcID :: String -> Int
calcID = calc 512

calc :: Int -> String -> Int    
calc _ [] = 0
calc n (x:xs) = num x + calc (n `div` 2) xs 
    where
        num 'B' = n
        num 'R' = n
        num _ = 0

second :: [Int] -> Int
second seats = head.filter helper $ [1..1023]
    where
        set = fromList seats
        helper x = member (x - 1) set && not (member x set) && member (x + 1) set
                
main :: IO ()
main = do
    input <- lines <$> readFile inputPath
    let seats = map calcID input
    printf "Silver star:\t%d\n" $ first seats
    printf "Gold star:  \t%d\n" $ second seats
