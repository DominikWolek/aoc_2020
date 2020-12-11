import Text.Printf
import Data.List.Split (splitOn)
import Data.Array

type Cords = (Int, Int)
type Seats = Array Cords Char
type AdjectantF = Seats -> Cords -> [Cords]
    
inputPath = "input"

occupied = ('#'==)
free = ('L'==)

checkOccupied input (y, x) f adjectant = f $ length $ filter (occupied.(input !)) (adjectant input (y, x))

maxY = fst.snd.bounds
maxX = snd.snd.bounds

takeSeats :: AdjectantF -> Int -> Seats -> Seats -> Cords -> Seats
takeSeats adjectant minOccupied input current (y, x)
    | y >= maxY input = current  
    | x >= maxX input = takeSeats adjectant minOccupied input current (y + 1, 1)
    | free (input ! (y, x)) && adjectantFree = nextX $ replaced '#'
    | occupied (input ! (y, x)) && adjectantOccupied = nextX $ replaced 'L'
    | otherwise = nextX current
    where
        adjectantFree = checkOccupied input (y, x) (== 0) adjectant
        adjectantOccupied = checkOccupied input (y, x) (>= minOccupied) adjectant
        replaced char = current // [((y, x), char)]
        nextX next = takeSeats adjectant minOccupied input next (y, x + 1)

repeatUntilSame :: (Seats -> Seats -> Cords -> Seats) -> Seats -> Int -> Int 
repeatUntilSame f input prev
    | prev == res = res
    | otherwise = repeatUntilSame f curr res
    where 
        curr = f input input (1, 1)
        res = length $ filter occupied $ elems curr 

second :: Seats -> Int
second input = repeatUntilSame (takeSeats adjectant 5) input 0
    where 
        addTillSeat input (y, x) (dirY, dirX)
            | x == maxX input || y == maxY input || x == 0 || y == 0 = (0,0)
            | input ! (y + dirY, x + dirX) == '.' = addTillSeat input (y + dirY, x + dirX) (dirY, dirX)
            | otherwise = (y + dirY, x + dirX)
        dirs = [(-1, 0), (1, 0), (0, -1), (0, 1), 
                (-1, -1), (-1, 1), (1, -1), (1, 1)]
        adjectant input (y, x) = map (addTillSeat input (y, x)) dirs

first :: Seats -> Int
first input = repeatUntilSame (takeSeats adjectant 4) input 0
    where 
        adjectant _ (y, x) = [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1), 
                              (y - 1, x - 1), (y - 1, x + 1), (y + 1, x - 1), (y + 1, x + 1)]

main :: IO ()
main = do
    parsed <- map (\x -> ('.':x)++".").lines <$> readFile inputPath
    let xLen = length.head $ parsed
    let input = (replicate xLen '.':parsed)++[replicate xLen '.']
    let yLen = length input
    let array = listArray ((0,0), (yLen - 1, (xLen - 1))) $ concat input
    printf "Silver star:\t%d\n" $ first array
    printf "Gold star:  \t%d\n" $ second array
