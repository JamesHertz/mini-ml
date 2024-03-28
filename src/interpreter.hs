module Interpreter (
    Value(..),
    eval,
)where

import Parser ( Ast(..), Token(..), Assigment  )
import qualified Data.Map as Map

type Enviroment = Map.Map String Value
data Value = IntValue Int | BoolValue Bool deriving (Eq)
instance Show Value where
    show (IntValue  x) = show x
    show (BoolValue x) = show x

eval :: Ast -> Value
eval ast = eval' ast Map.empty

eval' :: Ast -> Enviroment -> Value

-- TODO: make check more generic so you can use it for this c:
-- values
eval' (Number x) env = IntValue x
eval' (Bool x)   env = BoolValue x
-- arithmetic
eval' (Unary MINUS value) env = IntValue $ negate . evalInt value $ env 
eval' (Binary left PLUS  right) env = IntValue $ evalInt left env + evalInt right env
eval' (Binary left MINUS right) env = IntValue $ evalInt left env - evalInt right env
eval' (Binary left SLASH right) env = IntValue $ evalInt left env `div` evalInt right env
eval' (Binary left TIMES right) env = IntValue $ evalInt left env * evalInt right env
-- comparison
eval' (Binary left EQ_EQ right) env = BoolValue $ eval' left env == eval' right env
eval' (Binary left  N_EQ right) env = BoolValue $ eval' left env /= eval' right env
eval' (Binary left  GT'  right) env = BoolValue $ evalInt left env >  evalInt right env
eval' (Binary left  LT'  right) env = BoolValue $ evalInt left env <  evalInt right env
eval' (Binary left GT_EQ right) env = BoolValue $ evalInt left env >= evalInt right env
eval' (Binary left LT_EQ right) env = BoolValue $ evalInt left env <= evalInt right env
eval' (Binary left OR  right) env = BoolValue $ evalBool left env || evalBool right env
eval' (Binary left AND right) env = BoolValue $ evalBool left env && evalBool right env
eval' (Unary BANG expr) env = BoolValue $ not $ evalBool expr env

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