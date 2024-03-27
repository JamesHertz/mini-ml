module TypeChecker (
    Type(..),
    typeCheck
) where

-- TODO: improve overall error reporting c:
import Parser ( Ast(..) )
import qualified Data.Map as Map
import Control.Monad (foldM)
import Errors ( Result )

data Type = IntType | BoolType deriving (Eq)

type TypeEnv = Map.Map String Type
-- type Result  = Either String

instance Show Type where
    show IntType  = "integer"
    show BoolType = "boolean"

typeCheck :: Ast -> Result Type
typeCheck ast = typeCheck' ast Map.empty

typeCheck' :: Ast -> TypeEnv ->  Result Type
typeCheck' (Number x) env = Right IntType
typeCheck' (Bool x)   env = Right BoolType
typeCheck' (Minus x)  env = checkValue x env IntType

typeCheck' (Add  left right)  env = check left right env IntType IntType
typeCheck' (Sub  left right)  env = check left right env IntType IntType
typeCheck' (Div  left right)  env = check left right env IntType IntType
typeCheck' (Mult left right)  env = check left right env IntType IntType
typeCheck' (And  left right)  env = check left right env BoolType BoolType
typeCheck' (Or   left right)  env = check left right env BoolType BoolType

typeCheck' (GreaterThan   left right) env = check left right env IntType BoolType
typeCheck' (LessThan      left right) env = check left right env IntType BoolType
typeCheck' (GreaterThanEq left right) env = check left right env IntType BoolType
typeCheck' (LessThanEq    left right) env = check left right env IntType BoolType

typeCheck' (Equals left right) env     =  checkEquals left right env
typeCheck' (NotEquals left right) env  =  checkEquals left right env

-- TODO: make check more generic so you can use it for this c:
typeCheck' (Neg  expr) env = checkValue expr env BoolType 

-- handling identifier c:
typeCheck' (LetBlock assigns body) env = do
    newEnv <- foldM mapFunc env assigns
    typeCheck' body newEnv
    where 
        mapFunc map (name, value) =  do
            tt <- typeCheck' value map
            return $ Map.insert name tt map

typeCheck' (Var name) env = 
    case Map.lookup name env of
        Just value -> return value
        Nothing -> Left $ "Undefined identifier: " ++ name

-- Helper function c:
check :: Ast -> Ast -> TypeEnv -> Type -> Type -> Result Type
check left right env expected result = do
    left'  <- typeCheck' left  env
    right' <- typeCheck' right env

    if left' == right' 
        && right' == expected 
    then return result
    else Left $ "Type error: Expected two " ++ show expected ++ "s but found: " 
                    ++ show left' ++ " and " ++ show right' 

checkValue :: Ast -> TypeEnv -> Type -> Result Type -- TODO: finish this
checkValue ast env expected = do
    exprT <- typeCheck' ast env
    if exprT == expected 
        then return expected
    else Left $ "Type error: Expected an " ++ show expected ++ " but found a " ++ show exprT

checkEquals :: Ast -> Ast -> TypeEnv -> Result Type
checkEquals left right env =  do
    left'  <- typeCheck' left env
    right' <- typeCheck' right env

    if left' == right' 
        then return BoolType
    else Left $ "Type error: Expected two booleans or two integers but found: " 
                      ++ show left' ++ " and " ++ show right'