import Text.Printf
import qualified Data.Set as S
import Data.Either
import Control.Monad.State

inputPath = "input"

type Instruction = (String, Int)
type OpState = (Int, Int)
type ExecReturn = (Either Int Int)
type ExecState = (S.Set Int, OpState)

fromLeft :: Either a b -> a
fromLeft = either id (error "Right")

fromRight :: Either a b -> b
fromRight = either (error "Left") id

first :: [Instruction] -> Int
first input = fromLeft $ evalState (exec input) (S.empty, (0, 0))

exec :: [Instruction] -> State ExecState ExecReturn
exec instructions = do
    (set, (acc, ptr)) <- get
    if ptr >= length instructions
        then return $ Right acc
        else if S.member ptr set
            then return $ Left acc
            else do
                execOne (instructions !! ptr)
                exec instructions

execOne :: Instruction -> State ExecState ()
execOne instruction = do
    (set, (acc, ptr)) <- get
    let opState = interpret instruction (acc, ptr)
    let newSet = S.insert ptr set
    put (newSet, opState)

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
        calc = evalState (exec newInstructions) (S.empty, (0,0))
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