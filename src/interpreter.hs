module Interpreter (
    Value(..),
    eval,
)where

import Parser ( Ast(..), AstNode(..), Token(..), Assigment(..)  )
import Scanner (TokenValue(..))
import qualified Data.Map as Map

type Enviroment = Map.Map String Value
data Value = IntValue Int | BoolValue Bool | UnitValue deriving (Eq)
instance Show Value where
    show (IntValue  x) = show x
    show (BoolValue x) = show x
    show UnitValue     = "()"

eval :: Ast -> Value
eval ast = eval' ast Map.empty

eval' :: Ast -> Enviroment -> Value

-- TODO: make check more generic so you can use it for this c:
-- values
eval' Ast { node = Number x } env = IntValue x
eval' Ast { node = Bool x }   env = BoolValue x
eval' Ast { node = Unit }     env = UnitValue
-- arithmetic
eval' Ast { node = Unary MINUS value } env = IntValue $ negate . evalInt value $ env 
eval' Ast { node = Binary left PLUS right  } env = IntValue $ evalInt left env + evalInt right env
eval' Ast { node = Binary left MINUS right } env = IntValue $ evalInt left env - evalInt right env
eval' Ast { node = Binary left SLASH right } env = IntValue $ evalInt left env `div` evalInt right env
eval' Ast { node = Binary left TIMES right } env = IntValue $ evalInt left env * evalInt right env
-- comparison
eval' Ast { node = Binary left EQ_EQ right } env = BoolValue $ eval' left env == eval' right env
eval' Ast { node = Binary left  N_EQ right } env = BoolValue $ eval' left env /= eval' right env
eval' Ast { node = Binary left  GT'  right } env = BoolValue $ evalInt left env >  evalInt right env
eval' Ast { node = Binary left  LT'  right } env = BoolValue $ evalInt left env <  evalInt right env
eval' Ast { node = Binary left GT_EQ right } env = BoolValue $ evalInt left env >= evalInt right env
eval' Ast { node = Binary left LT_EQ right } env = BoolValue $ evalInt left env <= evalInt right env
eval' Ast { node = Binary left OR  right } env = BoolValue $ evalBool left env || evalBool right env
eval' Ast { node = Binary left AND right } env = BoolValue $ evalBool left env && evalBool right env
eval' Ast { node = Unary NOT expr } env = BoolValue $ not $ evalBool expr env

eval' Ast { node = LetBlock assigns body } env = 
    let
        newEnv = foldl mapFunc env assigns
    in eval' body newEnv
    where 
        mapFunc map Assigment {varName, assignValue} = 
            Map.insert varName (eval' assignValue map)  map

eval' Ast { node = Var name } env = 
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
