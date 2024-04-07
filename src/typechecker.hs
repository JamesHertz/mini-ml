module TypeChecker (
    Type(..),
    typeCheck
) where

-- TODO: improve overall error reporting c:
import Parser ( Ast(..), AstNode(..), Token(..), Assigment(..))
import Scanner (Token(..), TokenValue(..))
import qualified Data.Map as Map
import Control.Monad (foldM)
import Errors  

data Type = IntType | BoolType | UnitType deriving (Eq)

type TypeEnv = Map.Map String Type

instance Show Type where
    show IntType  = "integer"
    show BoolType = "boolean"
    show UnitType = "()"

typeCheck :: Ast -> Result Type
typeCheck ast = typeCheck' ast Map.empty

typeCheck' :: Ast -> TypeEnv ->  Result Type
typeCheck' (Ast { node = Number x }) env = Right IntType
typeCheck' (Ast { node = Bool x })   env = Right BoolType
typeCheck' ast@(Ast { node = Unary MINUS x }) env = checkValue x env IntType

-- arithmetic operators
typeCheck' ast@(Ast { node = Binary _ PLUS  _ }) env = check ast env IntType IntType
typeCheck' ast@(Ast { node = Binary _ MINUS _ }) env = check ast env IntType IntType
typeCheck' ast@(Ast { node = Binary _ TIMES _ }) env = check ast env IntType IntType
typeCheck' ast@(Ast { node = Binary _ SLASH _ }) env = check ast env IntType IntType

-- logical operators
typeCheck' ast@(Ast { node = Binary _ AND _ }) env = check ast env BoolType BoolType
typeCheck' ast@(Ast { node = Binary _ OR  _ }) env = check ast env BoolType BoolType

-- comparison operatos 
typeCheck' ast@(Ast { node = Binary _  GT'  _ }) env = check ast env IntType BoolType
typeCheck' ast@(Ast { node = Binary _  LT'  _ }) env = check ast env IntType BoolType
typeCheck' ast@(Ast { node = Binary _ GT_EQ _ }) env = check ast env IntType BoolType
typeCheck' ast@(Ast { node = Binary _ LT_EQ _ }) env = check ast env IntType BoolType

typeCheck' ast@(Ast { node = Binary _ EQ_EQ _ }) env = checkEquals ast env
typeCheck' ast@(Ast { node = Binary _ N_EQ  _ }) env = checkEquals ast env

-- TODO: make check more generic so you can use it for this c:
typeCheck' ast@(Ast { node = Unary NOT _ }) env = checkValue ast env BoolType 

-- handling identifier c:
typeCheck' (Ast { node = LetBlock assigns body }) env = do
    newEnv <- foldM mapFunc env assigns
    typeCheck' body newEnv
    where 
        mapFunc map Assigment{ varName, assignValue, expectedType } = do 
            typ <- typeCheck' assignValue map
            maybe (insert typ) (\(typ', token) -> 
                if typ' == typ then 
                    insert typ
                else makeError token $ 
                        "Expected '" ++ show typ' ++ "' type, but expression produced '" ++ show typ ++ "' type."
                ) expectedType
            where 
                insert typ = return $ Map.insert varName typ map

typeCheck' (Ast { token, node = Var name }) env = 
    maybe (makeError token $ "Undefined identifier: " ++ name) 
            return $  Map.lookup name env 

-- Helper function c:
check :: Ast -> TypeEnv -> Type -> Type -> Result Type
check (Ast { token, node = Binary left _ right }) env expected result = do
    left'  <- typeCheck' left  env
    right' <- typeCheck' right env

    if left' == right' 
        && right' == expected 
    then return result
    else makeError token $ "Expected two '" ++ show expected ++ "' type but found: '" 
                    ++ show left' ++ "' type and '" ++ show right' ++ "' type."

checkValue :: Ast -> TypeEnv -> Type -> Result Type -- TODO: finish this
checkValue (Ast { token, node = Unary _ child }) env expected = do
    exprT <- typeCheck' child env
    if exprT == expected 
        then return expected
    else makeError token $ "Expected a/an '" 
            ++ show expected ++ "' type but found a/an '" ++ show exprT ++ "' type."

checkEquals :: Ast -> TypeEnv -> Result Type
checkEquals (Ast { token, node = Binary left _ right } ) env =  do
    left'  <- typeCheck' left env
    right' <- typeCheck' right env

    if left' == right' 
        then return BoolType
    -- TODO: think about the posibility of displaying the positions of the others types
    else makeError token $ "Expected two 'boolean' type or two 'integer' type but found: '" 
                              ++ show left' ++ "' type and '" ++ show right' ++ "' type."

makeError :: Token -> String -> Result a
makeError (Token { Scanner.line, Scanner.position }) msg = 
    Left $ Error { 
                   errType  = TypingError, 
                   message  = msg, 
            Errors.line     = line, 
            Errors.position = position 
        }

