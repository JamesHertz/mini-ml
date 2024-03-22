module Interpreter (
    Value(..),
    eval
)where

import Parser

data Value = IntValue Int | BoolValue Bool deriving (Eq, Show)

eval :: Ast -> Value
-- values
eval (Number x) = IntValue x
eval (Bool x)   = BoolValue x
-- arithmetic
eval (Neg value) = IntValue $ negate . evalInt $ value
eval (Add  left right) = IntValue  $ evalInt  left + evalInt right
eval (Sub  left right) = IntValue  $ evalInt  left - evalInt right
eval (Div  left right) = IntValue  $ evalInt  left `div` evalInt right
eval (Mult left right) = IntValue  $ evalInt  left * evalInt right
-- comparison
eval (Equals        left right) = BoolValue $ eval left == eval right
eval (GreaterThan   left right) = BoolValue $ evalInt left >  evalInt right
eval (LessThan      left right) = BoolValue $ evalInt left <  evalInt right
eval (GreaterThanEq left right) = BoolValue $ evalInt left >= evalInt right
eval (LessThanEq    left right) = BoolValue $ evalInt left <= evalInt right
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