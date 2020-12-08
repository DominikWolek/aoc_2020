import Text.Printf
import qualified Data.Set as S
import Data.Either
import Control.Monad.State
import Data.Array

inputPath = "input"

type Instruction = (String, Int)
type Instructions = Array Int Instruction
type OpState = (Int, Int)
-- Left acc: infinite loop
-- Right acc: finished
type ExecReturn = (Either Int Int)
type ExecState = (S.Set Int, OpState)

exec :: Instructions -> State ExecState ExecReturn
exec instructions = do
    (set, (acc, i)) <- get
    if i >= length instructions
        then return $ Right acc
        else if S.member i set
            then return $ Left acc
            else do
                execOne (instructions ! i)
                exec instructions

execOne :: Instruction -> State ExecState ()
execOne instruction = do
    (set, (acc, i)) <- get
    let opState = interpret instruction (acc, i)
    let newSet = S.insert i set
    put (newSet, opState)

interpret :: Instruction -> OpState -> OpState
interpret ("acc", n) (acc, i) = (acc + n, i + 1)
interpret ("jmp", n) (acc, i) = (acc, i + n)
interpret ("nop", _) (acc, i) = (acc, i + 1)

first :: Instructions -> Int
first instructions = head.lefts $ [evalState (exec instructions) (S.empty, (0, 0))]

change :: Instruction -> Instruction
change ("acc", n) = ("acc", n)
change ("jmp", n) = ("nop", n)
change ("nop", n) = ("jmp", n)

second :: Instructions -> Int
second instructions = head.rights.map (changeN instructions) $ [0..]

changeN :: Instructions -> Int -> ExecReturn
changeN instructions n =
    evalState (exec newInstructions) (S.empty, (0,0))
    where
        instruction = change (instructions ! n)
        newInstructions = instructions // [(n, instruction)]

parse :: String -> Instruction
parse (i1:i2:i3:_:'+':n) = (i1:i2:[i3], read n::Int)
parse (i1:i2:i3:_:'-':n) = (i1:i2:[i3], read ('-':n)::Int)

main :: IO ()
main = do
    input <- map parse.lines <$> readFile inputPath
    let instructions = listArray (0, (length input) - 1) input
    printf "Silver star:\t%d\n" $ first instructions
    printf "Gold star:  \t%d\n" $ second instructions