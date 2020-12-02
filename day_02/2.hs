import Text.Printf
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Data.Either

type PasswdReqt = ((Int, Int), Char, String)

inputPath = "input"

first :: [PasswdReqt] -> Int
first = length.filter checkReqt1

checkReqt1 :: PasswdReqt -> Bool
checkReqt1 ((lower, upper), char, passwd) = lower <= number && number <= upper
    where
        number = length $ filter (==char) passwd

second :: [PasswdReqt] -> Int
second = length.filter checkReqt2

checkReqt2 :: PasswdReqt -> Bool
checkReqt2 ((lower, upper), char, passwd) = first /= second
    where 
        first = (passwd !! (lower - 1)) == char
        second =  (passwd !! (upper - 1)) == char

parser :: GenParser Char st PasswdReqt
parser = do
    lower <- many digit
    _ <- char '-'
    upper <- many digit
    _ <- char ' '
    char <- letter
    _ <- string ": "
    passwd <- many letter
    return ((read lower, read upper), char, passwd)

main :: IO ()
main = do
    input <- rights.map (parse parser inputPath).lines <$> readFile inputPath
    printf "Silver star:\t%d\n" $ first input
    printf "Gold star:  \t%d\n" $ second input
