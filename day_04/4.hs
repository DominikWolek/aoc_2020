import Text.Printf
import Data.List.Split (splitOn)
import Data.Char

type Passport = [(String, String)]

inputPath = "input"
requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
eyeColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

parse :: String -> Passport
parse str = map (\x -> (take 3 x, drop 4 x)) $ words str

first :: Passport -> Bool
first passport = all (`elem` fields) requiredFields where fields = map fst passport

validReqt :: (String, String) -> Bool
validReqt ("byr", x) = year <= 2002 && year >= 1920 where year = read x::Int
validReqt ("iyr", x) = year <= 2020 && year >= 2010 where year = read x::Int
validReqt ("eyr", x) = year <= 2030 && year >= 2020 where year = read x::Int
validReqt ("hgt", x:(y:"in")) = height <= 76 && height >= 59 where height = read [x,y]::Int
validReqt ("hgt", x:(y:(z:"cm"))) = height <= 193 && height >= 150 where height = read [x,y,z]::Int
validReqt ("hcl", '#':x) = length x == 6 && all isHexDigit x
validReqt ("ecl", x) = x `elem` eyeColors
validReqt ("pid", x) = length x == 9 && all isDigit x
validReqt ("cid", _) = True
validReqt (_, _) = False

second :: Passport -> Bool
second = all validReqt

main :: IO ()
main = do    
    input <- map parse.splitOn "\n\n" <$> readFile inputPath
    let after_first = filter first input
    let after_second = filter second after_first
    printf "Silver star:\t%d\n" $ length after_first
    printf "Gold star:  \t%d\n" $ length after_second