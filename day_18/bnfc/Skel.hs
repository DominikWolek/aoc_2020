-- Haskell module generated by the BNF converter

module Skel where

import qualified Abs

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transIn :: Abs.In -> Result
transIn x = case x of
  Abs.Input stmts -> failure x
transStmt :: Abs.Stmt -> Result
transStmt x = case x of
  Abs.SExp exp -> failure x
transExp :: Abs.Exp -> Result
transExp x = case x of
  Abs.ExpAdd exp1 exp2 -> failure x
  Abs.ExpMul exp1 exp2 -> failure x
  Abs.ExpLit integer -> failure x

