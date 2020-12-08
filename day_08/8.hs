import Text.Printf
import Data.Set (empty)
import Data.Either (rights, lefts)
import Control.Monad.State (evalState)
import Data.Array
import Interpreter

inputPath = "input"

first :: Instructions -> Int
first instructions = head.lefts $ [evalState (exec instructions) (empty, (0, 0))]

change :: Instruction -> Instruction
change ("acc", n) = ("acc", n)
change ("jmp", n) = ("nop", n)
change ("nop", n) = ("jmp", n)

second :: Instructions -> Int
second instructions = head.rights.map (changeN instructions) $ [0..]

changeN :: Instructions -> Int -> ExecReturn
changeN instructions n =
    evalState (exec newInstructions) (empty, (0,0))
    where
        instruction = change (instructions ! n)
        newInstructions = instructions // [(n, instruction)]

main :: IO ()
main = do
    input <- map parseInstruction.lines <$> readFile inputPath
    let instructions = listArray (0, (length input) - 1) input
    printf "Silver star:\t%d\n" $ first instructions
    printf "Gold star:  \t%d\n" $ second instructions