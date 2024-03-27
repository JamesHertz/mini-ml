module Interpreter (
    Value(..),
    eval,
)where

import Parser ( Ast(..), Assigment )
import qualified Data.Map as Map

type Enviroment = Map.Map String Value
data Value = IntValue Int | BoolValue Bool deriving (Eq, Show)

eval :: Ast -> Value
eval ast = eval' ast Map.empty

eval' :: Ast -> Enviroment -> Value
-- values
eval' (Number x) env = IntValue x
eval' (Bool x)   env = BoolValue x
-- arithmetic
eval' (Minus value) env = IntValue $ negate . evalInt value $ env 
eval' (Add  left right) env = IntValue $ evalInt left env + evalInt right env
eval' (Sub  left right) env = IntValue $ evalInt left env - evalInt right env
eval' (Div  left right) env = IntValue $ evalInt left env `div` evalInt right env
eval' (Mult left right) env = IntValue $ evalInt left env * evalInt right env
-- comparison
eval' (Equals        left right) env = BoolValue $ eval' left env == eval' right env
eval' (NotEquals     left right) env = BoolValue $ eval' left env /= eval' right env
eval' (GreaterThan   left right) env = BoolValue $ evalInt left env >  evalInt right env
eval' (LessThan      left right) env = BoolValue $ evalInt left env <  evalInt right env
eval' (GreaterThanEq left right) env = BoolValue $ evalInt left env >= evalInt right env
eval' (LessThanEq    left right) env = BoolValue $ evalInt left env <= evalInt right env
eval' (Or   left right) env = BoolValue $ evalBool left env || evalBool right env
eval' (And  left right) env = BoolValue $ evalBool left env && evalBool right env
eval' (Neg expr) env = BoolValue $ not $ evalBool expr env

eval' (LetBlock assigns body) env = 
    let
        newEnv = foldl mapFunc env assigns
    in eval' body newEnv
    where 
        mapFunc map (name, value) = Map.insert name (eval' value map)  map

eval' (Var name) env = 
    case Map.lookup name env of
        Just value -> value
        Nothing -> error "Something is wrong"

evalInt :: Ast -> Enviroment -> Int 
evalInt ast env = 
    let 
        (IntValue result) = eval' ast env
    in result

evalBool :: Ast -> Enviroment -> Bool
evalBool ast env = 
    let 
        (BoolValue result) = eval' ast env
    in result