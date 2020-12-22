import Text.Printf
import Data.List.Split (splitOn)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Data.Either

inputPath = "input"

type Deck = [Int]
type Decks = (Deck, Deck)

parse :: String -> Decks
parse = (\[x, y] -> (x, y)).map (map read.tail.lines).splitOn "\n\n"

calcScore :: [Int] -> Int
calcScore = sum.zipWith (*) [1..].reverse

fstWin (x:xs, y:ys) = (xs ++ [x, y], ys)
sndWin (x:xs, y:ys) = (xs, ys ++ [y, x])

combat :: Decks -> Decks
combat decks@(x:xs, y:ys) 
    | x > y = fstWin decks
    | x < y = sndWin decks

first :: ([Int], [Int]) -> Int
first ([], p2) = calcScore p2
first (p1, []) = calcScore p1
first decks = first.combat $ decks

recursiveCombat :: Decks -> Decks
recursiveCombat decks@(x:xs, y:ys)
    | x > length xs || y > length ys = combat decks
    | otherwise = either 
        (\_ -> fstWin decks)
        (\_ -> sndWin decks)
        (recursiveGame S.empty (take x xs, take y ys))

recursiveGame :: S.Set Decks -> Decks -> Either Deck Deck
recursiveGame _ ([], deck) = Right deck
recursiveGame _ (deck, []) = Left deck
recursiveGame set decks@(p1, _)
    | S.member decks set = Left p1
    | otherwise = recursiveGame (S.insert decks set) (recursiveCombat decks)

second :: Decks -> Int
second = either calcScore calcScore.recursiveGame S.empty

main :: IO ()
main = do
    decks <- parse <$> readFile inputPath
    printf "Silver star:\t%d\n" $ first decks
    printf "Gold star:  \t%d\n" $ second decks
