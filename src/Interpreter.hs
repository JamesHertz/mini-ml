{-# LANGUAGE RecordWildCards #-}
module Interpreter (
    Value(..),
    eval,
)where

import Parser (Token(..), Assigment(..), Parameter )
import Types (TypedAst, Ast(..), AstNode(..))
import Scanner (TokenValue(..))
import qualified Data.Map as Map
import Control.Monad (foldM)
import Control.Monad.State (StateT, evalStateT, liftIO, gets, modify)
import System.IO (hFlush, stdout)
import Data.IntMap (insert)
import Data.Bifunctor (second)
import Text.Printf (printf)
import TypeChecker (Type(UnitType))
import Data.Tuple.Extra (fst3)

type Enviroment  = Map.Map String Value

type Address     = Int
type MemoryCells = Map.Map Address Value

type ParName = String
data Value =  IntValue Int 
            | BoolValue Bool 
            | Ref Address 
            | UnitValue 
            | Closure { 
                env            :: Enviroment, 
                parametersName :: [ParName], 
                funcBody       :: TypedAst,
                extraArgs      :: [Value] -- used for partial application c: 
              } deriving (Eq)

instance Show Value where
    show (IntValue  x) = show x
    show (BoolValue x) = show x
-- TODO: when the program starts generate a random address to start with
-- and on each step choose a range to get the next address.
    show (Ref x)       = "Ref 0x" ++ printf "%x" x
    show UnitValue     = "()"
    show Closure {}    = "yeah-its-a-function" -- TODO: make it print the function type  ....


type EvalState = StateT (Address, MemoryCells) IO

eval :: TypedAst -> IO Value
eval ast = evalStateT (eval' ast Map.empty) (0, Map.empty)

eval' :: TypedAst -> Enviroment -> EvalState Value
eval' Ast { node = Number x } env = return $ IntValue x
eval' Ast { node = Bool x }   env = return $ BoolValue x
eval' Ast { node = Unit }     env = return UnitValue

-- unaries
eval' Ast { node = Unary MINUS value } env = IntValue . negate <$> evalInt value env
eval' Ast { node = Unary NOT expr    } env = BoolValue . not <$> evalBool expr env
eval' Ast { node = Unary NEW value   } env = eval' value env >>= reserveCell
eval' Ast { node = Unary BANG value  } env = do 
    result <- eval' value env 
    let (Ref address) = result
    getValue address

eval' Ast { node = Unary PRINT value } env = do
    result <- eval' value env
    liftIO $ putStr $ show result
    liftIO $ hFlush stdout
    return UnitValue

eval' Ast { node = Unary PRINTLN value } env = do
    result <- eval' value env
    liftIO $ print result
    return UnitValue

-- Binaries
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

-- FIXME: and and or should not evaluate both sides always
eval' ast@Ast { node = Binary left OR  right } env = evalBinary evalBool (||) BoolValue ast env
eval' ast@Ast { node = Binary left AND right } env = evalBinary evalBool (&&) BoolValue ast env

-- dealing with identifiers
eval' Ast { node = LetBlock assigns body } env = do
     newEnv <- foldM mapFunc env assigns
     eval' body newEnv
    where 
        mapFunc map Assigment {varName, assignValue = func@(Ast { node = FuncDecl _ _ }) } =  do
            value <- eval' func map
            let 
              Closure { env, parametersName, funcBody } = value
              -- sending state from the future to the past
              newMap = Map.insert varName Closure {
                    extraArgs = [],
                    env       = newMap,
                    parametersName,
                    funcBody
                } map
            return newMap

        mapFunc map Assigment {varName, assignValue} =  do
            value <- eval' assignValue map
            return $ Map.insert varName value  map

eval' Ast { node = Var name } env = 
    case Map.lookup name env of
        Just value -> return value
        Nothing    -> error $ printf "BUG!! Unable to resolve variable %s" name 

eval' Ast { node = RefAssignment ref value } env = do
    result <- eval' ref env
    value' <- eval' value env

    let (Ref address) = result
    setValue address value'
    return value'

-- control flow
eval' Ast { node = If { condition, body, elseBody } } env = do
    condValue <- evalBool condition env
    let resultValue = if condValue then eval' body env
                      else maybe (return UnitValue) (`eval'` env) elseBody
    maybe (return UnitValue) (const resultValue) elseBody

eval' Ast { node = Sequence fst snd } env = do
    eval' fst env
    eval' snd env

eval' ast@Ast { node = While cond body } env = do
    result <- evalBool cond env
    if result then do
        eval' body env
        eval' ast env
    else return UnitValue

eval' Ast { node = FuncDecl pars funcBody } env = do
    let parametersName = fst3 <$> pars
    return Closure { 
           parametersName, env, funcBody, extraArgs = []
      }

eval' Ast { node = Call func pars } env = do 
    closure   <- eval' func env
    parsValue <- mapM (`eval'` env) pars
    let 
        Closure { env = closureEnv, parametersName = parsName, funcBody, extraArgs } = closure
        funcArgs = extraArgs ++ parsValue

    newCloseEnv <- foldM addToEnv closureEnv $ zip parsName funcArgs
    eval' funcBody newCloseEnv

eval' Ast { node = PartialApplication func pars } env = do 
    closure   <- eval' func env
    parsValue <- mapM (`eval'` env) pars
    let 
        Closure { env = closureEnv, parametersName = parsName, .. } = closure
    return Closure { 
        env = closureEnv, 
        parametersName = parsName, 
        extraArgs = extraArgs ++ parsValue,
        funcBody
     }

-- memory helper functions
reserveCell :: Value -> EvalState Value
reserveCell value = do
    address <- gets fst
    modify $ \(_, map) -> (address + 1, Map.insert address value map)
    return $ Ref address

getValue :: Address -> EvalState Value
getValue address = do
    cell <- gets (Map.lookup address . snd)
    case cell of
        Just value -> return value
        Nothing -> error $ "BUG couldn't get value for: " ++ show address 

-- TODO: think if it is worthed to use the suggestion c:
setValue :: Address -> Value -> EvalState ()
setValue address value = modify $ second (Map.insert address value)
    
-- helper functions
evalBinary :: (TypedAst -> Enviroment -> EvalState a) -> (a -> a -> b) -> (b -> c) -> TypedAst -> Enviroment -> EvalState c
evalBinary evaluator operation wrapper Ast { node = Binary left _ right } env = do
    left'  <- evaluator left env
    right' <- evaluator right env
    return $ wrapper $ left' `operation` right'

evalInt :: TypedAst -> Enviroment -> EvalState Int 
evalInt ast env = do
    value <- eval' ast env
    let (IntValue result) = value
    return result

evalBool :: TypedAst -> Enviroment -> EvalState Bool
evalBool ast env = do
    value <- eval' ast env
    let (BoolValue result) = value
    return result

-- small function used in other places c:
addToEnv :: Enviroment -> (String, Value) ->  EvalState Enviroment
addToEnv currEnv (name, value) = 
  return $ Map.insert name value currEnv
