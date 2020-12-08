import Text.Printf
import Data.Text.Read
import qualified Data.Set as S
import Data.Either

inputPath = "input"

type Instruction = (String, Int)
type OpState = (Int, Int)

fromLeft :: Either a b -> a
fromLeft = either id (error "Right")

fromRight :: Either a b -> b
fromRight = either (error "Left") id

first :: [Instruction] -> Int
first input = fromLeft $ exec input S.empty (0, 0)

exec :: [Instruction] -> S.Set Int -> OpState -> Either Int Int
exec instructions set (acc, ptr) =
    if ptr >= length instructions
        then Right acc
        else if S.member ptr set
            then Left acc
            else exec instructions newSet newState
    where
        newState = interpret (instructions !! ptr) (acc, ptr)
        newSet = S.insert ptr set

change :: Instruction -> Instruction
change ("acc", n) = ("acc", n)
change ("jmp", n) = ("nop", n)
change ("nop", n) = ("jmp", n)

second :: [Instruction] -> Int -> Int
second input n =
    if (fst instruction == "acc") || continue
        then second input (n + 1)
        else fromRight calc
    where
        instruction = change (input !! n)
        newInstructions = take n input ++ (instruction: drop (n + 1) input)
        calc = exec newInstructions S.empty (0,0)
        continue = isLeft calc

parse :: String -> Instruction
parse (i1:i2:i3:_:'+':n) = (i1:i2:[i3], read n::Int)
parse (i1:i2:i3:_:'-':n) = (i1:i2:[i3], read ('-':n)::Int)

interpret :: Instruction -> OpState -> OpState
interpret ("acc", n) (acc, ptr) = (acc + n, ptr + 1)
interpret ("jmp", n) (acc, ptr) = (acc, ptr + n)
interpret ("nop", _) (acc, ptr) = (acc, ptr + 1)

main :: IO ()
main = do
    input <- map parse.lines <$> readFile inputPath
    printf "Silver star:\t%d\n" $ first input
    printf "Gold star:  \t%d\n" $ second input 0