module Interpreter (
    Value(..),
    eval
)where

import Parser

data Value = IntValue Int | BoolValue Bool deriving (Eq, Show)

eval :: Ast -> Value
eval (Number x) = IntValue x
eval (Bool x)   = BoolValue x
eval (Add  left right) = IntValue  $ evalInt  left + evalInt right
eval (Sub  left right) = IntValue  $ evalInt  left - evalInt right
eval (Div  left right) = IntValue  $ evalInt  left `div` evalInt right
eval (Mult left right) = IntValue  $ evalInt  left * evalInt right
eval (Or   left right) = BoolValue $ evalBool left || evalBool right
eval (And  left right) = BoolValue $ evalBool left && evalBool right

evalInt :: Ast -> Int 
evalInt ast = 
    let 
        (IntValue result) = eval ast
    in result

evalBool :: Ast -> Bool
evalBool ast = 
    let 
        (BoolValue result) = eval ast
    in result