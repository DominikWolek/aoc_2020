import Text.Printf
import Data.List.Split

type PasswdReqt = ((Int, Int), Char, String)

inputPath = "input"

first :: [PasswdReqt] -> Int
first = length.filter check_reqt_1

check_reqt_1 :: PasswdReqt -> Bool
check_reqt_1 ((lower, upper), char, passwd) = lower <= number && number <= upper
    where
        number = length $ filter (==char) passwd

second :: [PasswdReqt] -> Int
second = length.filter check_reqt_2

check_reqt_2 :: PasswdReqt -> Bool
check_reqt_2 ((lower, upper), char, passwd) = first <+> second
    where 
        first = (passwd !! (lower - 1)) == char
        second =  (passwd !! (upper - 1)) == char
        l <+> r = (l && not r) || (not l && r)
        
parse :: String -> PasswdReqt
parse line = ((read first::Int, read second::Int), head chars, passwd)
    where
        (first:[rest]) = splitOn "-" line
        (second:(chars:[passwd])) = splitOn " " rest
    
main :: IO ()
main = do
    input <- map parse.lines <$> readFile inputPath
    printf "Silver star:\t%d\n" $ first input
    printf "Gold star:\t%d\n" $ second input
