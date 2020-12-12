import Text.Printf

type Position = (Int, Int)
type Direction = (Int, Int)
type Instruction = (Char, Int)
type State = (Position, Direction)

inputPath = "input"

rotate :: Char -> Int -> Direction -> Direction
rotate 'L' val dir = iterate (\(x, y) -> (-y, x)) dir !! (val `div` 90)
rotate 'R' val dir = iterate (\(x, y) -> (y, -x)) dir !! (val `div` 90)

step1 :: State -> Instruction -> State
step1 ((x, y), (xDir, yDir)) (ins, val)
    | ins == 'N' = ((x, y + val), (xDir, yDir))
    | ins == 'E' = ((x + val, y), (xDir, yDir))
    | ins == 'W' = ((x - val, y), (xDir, yDir))
    | ins == 'S' = ((x, y - val), (xDir, yDir))
    | ins == 'F' = ((x + (xDir * val), y + (yDir * val)), (xDir, yDir))
    | ins == 'L' || ins == 'R' = ((x, y), (rotate ins val (xDir, yDir)))

first :: [Instruction] -> Int
first input = abs(x) + abs(y)
    where
        ((x, y), _) = foldl step1 ((0, 0), (1, 0)) input
    
step2 :: State -> Instruction -> State
step2 ((x, y), (xWay, yWay)) (ins, val)
    | ins == 'N' = ((x, y), (xWay, yWay + val))
    | ins == 'E' = ((x, y), (xWay + val, yWay))
    | ins == 'W' = ((x, y), (xWay - val, yWay))
    | ins == 'S' = ((x, y), (xWay, yWay - val))
    | ins == 'F' = ((x + (xWay * val), y + (yWay * val)), (xWay, yWay))
    | ins == 'L' || ins == 'R' = ((x, y), (rotate ins val (xWay, yWay)))

second :: [Instruction] -> Int
second input = abs(x) + abs(y)
    where
        ((x, y), _) = foldl step2 ((0, 0), (10, 1)) input

parse :: String -> Instruction
parse (x:n) = (x, read n)

main :: IO ()
main = do
    input <- map parse.lines <$> readFile inputPath
    printf "Silver star:\t%d\n" $ first input
    printf "Gold star:  \t%d\n" $ second input
