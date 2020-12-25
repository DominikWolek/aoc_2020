import Text.Printf
import Data.List.Split (splitOn)
import Data.List
import Data.Maybe

remainder = 20201227
initialSubject = 7
inputPath = "input"

func :: Int -> [Int]
func subject = iterate (doFunc subject) 1 
    where
        doFunc subject curr = (curr * subject) `mod` remainder

brute :: Int -> Int -> Int
brute end = fromJust.elemIndex end.func 

first :: Int -> Int -> Int
first cardPublic doorPublic = (!!cardLoop) $ func doorPublic
    where
        cardLoop = brute cardPublic initialSubject

main :: IO ()
main = do
    [cardPublic, doorPublic] <- map read.lines <$> readFile inputPath
    printf "Silver star:\t%d\n" $ first cardPublic doorPublic
    putStrLn "MERRY CHRISTMAS EVERYONE"