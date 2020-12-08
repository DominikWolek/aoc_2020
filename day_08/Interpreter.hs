module Interpreter (
    Instruction, 
    Instructions, 
    ExecReturn, 
    ExecState, 
    exec,
    parseInstruction
) where

import Data.Set (Set, insert, member)
import Data.Either
import Control.Monad.State
import Data.Array

type Instruction = (String, Int)
type Instructions = Array Int Instruction
type OpState = (Int, Int)
-- Left acc: infinite loop
-- Right acc: finished
type ExecReturn = (Either Int Int)
type ExecState = (Set Int, OpState)

exec :: Instructions -> State ExecState ExecReturn
exec instructions = do
    (set, (acc, i)) <- get
    if i >= length instructions
        then return $ Right acc
        else if member i set
            then return $ Left acc
            else do
                execOne (instructions ! i)
                exec instructions

execOne :: Instruction -> State ExecState ()
execOne instruction = do
    (set, (acc, i)) <- get
    let opState = interpret instruction (acc, i)
    let newSet = insert i set
    put (newSet, opState)

interpret :: Instruction -> OpState -> OpState
interpret ("acc", n) (acc, i) = (acc + n, i + 1)
interpret ("jmp", n) (acc, i) = (acc, i + n)
interpret ("nop", _) (acc, i) = (acc, i + 1)

parseInstruction :: String -> Instruction
parseInstruction (i1:i2:i3:_:'+':n) = (i1:i2:[i3], read n)
parseInstruction (i1:i2:i3:_:'-':n) = (i1:i2:[i3], read ('-':n))
