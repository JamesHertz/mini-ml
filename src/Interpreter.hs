module Interpreter (
    Value(..),
    eval,
)where

import Parser ( Ast(..), AstNode(..), Token(..), Assigment(..)  )
import Scanner (TokenValue(..))
import qualified Data.Map as Map
import Control.Monad (foldM)
import System.IO (hFlush, stdout)
import Data.IntMap (insert)

type Enviroment = Map.Map String Value
data Value = IntValue Int | BoolValue Bool | UnitValue deriving (Eq)
instance Show Value where
    show (IntValue  x) = show x
    show (BoolValue x) = show x
    show UnitValue     = "()"


{-
 
type Address = Int
data Value = 
      IntValue Int 
    | BoolValue Bool
    | UnitValue
    | Ref Address

-- data MemoryCells = Map.Map Address Value

data RefCell = {
    value    :: Value,
    refCount :: Int
}

data MemoryCells = Map.Map Address RefCell

THINK: 
    - How to do garbage collector??
    - How to handle null refs ?


type InterpreterState = State MemoryCells

eval' :: Ast -> Enviroment -> InterpreterState (IO Value)
eval' Ast { node = Unary NEW value } env = do
    result  <- eval' value env
    address <- insertCell value
    return . return . Ref $ address
    
insertCell :: Value -> InterpreterState Address
insertCell value = do 
    address <- nexAddress
    modify ....
    return address
    
-}

eval :: Ast -> IO Value
eval ast = eval' ast Map.empty

eval' :: Ast -> Enviroment -> IO Value

-- TODO: make check more generic so you can use it for this c:
-- values
eval' Ast { node = Number x } env = return $ IntValue x
eval' Ast { node = Bool x }   env = return $ BoolValue x
eval' Ast { node = Unit }     env = return UnitValue
-- arithmetic
eval' Ast { node = Unary MINUS value } env = IntValue . negate <$> evalInt value env

eval' ast@Ast { node = Binary left PLUS right  } env = evalBinary evalInt (+) IntValue ast env
eval' ast@Ast { node = Binary left MINUS right } env = evalBinary evalInt (-) IntValue ast env
eval' ast@Ast { node = Binary left SLASH right } env = evalBinary evalInt div IntValue ast env
eval' ast@Ast { node = Binary left TIMES right } env = evalBinary evalInt (*) IntValue ast env

-- comparison
eval' ast@Ast { node = Binary left EQ_EQ right } env = evalBinary eval'   (==) BoolValue ast env
eval' ast@Ast { node = Binary left  N_EQ right } env = evalBinary eval'   (/=) BoolValue ast env
eval' ast@Ast { node = Binary left  GT'  right } env = evalBinary evalInt (>)  BoolValue ast env
eval' ast@Ast { node = Binary left  LT'  right } env = evalBinary evalInt (<)  BoolValue ast env
eval' ast@Ast { node = Binary left GT_EQ right } env = evalBinary evalInt (>=) BoolValue ast env
eval' ast@Ast { node = Binary left LT_EQ right } env = evalBinary evalInt (<=) BoolValue ast env
eval' ast@Ast { node = Binary left OR  right } env = evalBinary evalBool (||) BoolValue ast env
eval' ast@Ast { node = Binary left AND right } env = evalBinary evalBool (&&) BoolValue ast env
eval' Ast { node = Unary NOT expr } env = BoolValue . not <$> evalBool expr env

eval' Ast { node = LetBlock assigns body } env = do
     newEnv <- foldM mapFunc env assigns
     eval' body newEnv
    where 
        mapFunc map Assigment {varName, assignValue} =  do
            value <- eval' assignValue map
            return $ Map.insert varName value  map

eval' Ast { node = Var name } env = 
    case Map.lookup name env of
        Just value -> return value
        Nothing -> error "Something is wrong"

eval' Ast { node = If { condition, body, elseBody } } env = do
    condValue <- evalBool condition env
    let resultValue = if condValue then eval' body env
                      else maybe (return UnitValue) (`eval'` env) elseBody
    maybe (return UnitValue) (const resultValue) elseBody

eval' Ast { node = Sequence fst snd } env = do
    eval' fst env
    eval' snd env


eval' Ast { node = Unary PRINT value } env = do
    result <- eval' value env
    putStr $ show result
    hFlush stdout
    return UnitValue

eval' Ast { node = Unary PRINTLN value } env = do
    result <- eval' value env
    print result
    return UnitValue

-- helper functions
evalBinary :: (Ast -> Enviroment -> IO a) -> (a -> a -> b) -> (b -> c) -> Ast -> Enviroment -> IO c
evalBinary evaluator operation wrapper Ast { node = Binary left _ right } env = do
    left'  <- evaluator left env
    right' <- evaluator right env
    return $ wrapper $ left' `operation` right'

evalInt :: Ast -> Enviroment -> IO Int 
evalInt ast env = do
    value <- eval' ast env
    let (IntValue result) = value
    return result

evalBool :: Ast -> Enviroment -> IO Bool
evalBool ast env = do
    value <- eval' ast env
    let (BoolValue result) = value
    return result
