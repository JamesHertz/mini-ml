module TypeChecker (
    Type(..),
    typeCheck
) where

-- TODO: improve overall error reporting c:
import Parser ( Ast(..), AstNode(..), Token(..), Assigment(..))
import Scanner (Token(..), TokenValue(..))
import qualified Data.Map as Map
import Control.Monad (foldM, when)
import Errors  

data Type = IntType | BoolType | UnitType | RefType Type deriving (Eq)

type TypeEnv = Map.Map String Type

instance Show Type where
    show IntType       = "int"
    show BoolType      = "bool"
    show UnitType      = "unit"
    show (RefType typ) = "ref " ++ show typ

typeCheck :: Ast -> Result Type
typeCheck ast = typeCheck' ast Map.empty

typeCheck' :: Ast -> TypeEnv ->  Result Type
typeCheck' Ast { node = Number x } env = return IntType
typeCheck' Ast { node = Bool x }   env = return BoolType
typeCheck' Ast { node = Unit }     env = return UnitType

-- unaries
typeCheck' Ast { node = Unary MINUS x } env   = checkValue x env IntType
typeCheck' Ast { node = Unary NOT value } env = checkValue value env BoolType 
typeCheck' Ast { node = Unary PRINTLN value } env = typeCheck' value env >> return UnitType
typeCheck' Ast { node = Unary PRINT   value } env = typeCheck' value env >> return UnitType
typeCheck' Ast { node = Unary NEW     value } env = RefType <$> typeCheck' value env
typeCheck' Ast { node = Unary BANG    value, token } env = do
    typ <- typeCheck' value env
    case typ of
        RefType t -> return t
        _ -> makeError token $ "Expected 'ref T' type but found '" ++ show typ ++ "' type."
    
-- binaries
-- arithmetic operators
typeCheck' ast@Ast { node = Binary _ PLUS  _ } env = checkBinary ast env IntType IntType
typeCheck' ast@Ast { node = Binary _ MINUS _ } env = checkBinary ast env IntType IntType
typeCheck' ast@Ast { node = Binary _ TIMES _ } env = checkBinary ast env IntType IntType
typeCheck' ast@Ast { node = Binary _ SLASH _ } env = checkBinary ast env IntType IntType

-- logical operators
typeCheck' ast@Ast { node = Binary _ AND _ } env = checkBinary ast env BoolType BoolType
typeCheck' ast@Ast { node = Binary _ OR  _ } env = checkBinary ast env BoolType BoolType

-- comparison operatos 
typeCheck' ast@Ast { node = Binary _  GT'  _ } env = checkBinary ast env IntType BoolType
typeCheck' ast@Ast { node = Binary _  LT'  _ } env = checkBinary ast env IntType BoolType
typeCheck' ast@Ast { node = Binary _ GT_EQ _ } env = checkBinary ast env IntType BoolType
typeCheck' ast@Ast { node = Binary _ LT_EQ _ } env = checkBinary ast env IntType BoolType

typeCheck' ast@Ast { node = Binary _ EQ_EQ _ } env = checkEquals ast env
typeCheck' ast@Ast { node = Binary _ N_EQ  _ } env = checkEquals ast env

-- handling identifier c:
typeCheck' Ast { node = LetBlock assigns body } env = do
    newEnv <- foldM mapFunc env assigns
    typeCheck' body newEnv
    where 
        mapFunc map Assigment { varName, assignValue, expectedType } = do 
            typ <- typeCheck' assignValue map
            maybe (insert typ) (\(typ', token) -> 
                if typ' == typ then 
                    insert typ
                else makeError token $ 
                        "Expected '" ++ show typ' ++ "' type, but expression produced '" ++ show typ ++ "' type."
                ) expectedType
            where 
                insert typ = return $ Map.insert varName typ map

typeCheck' Ast { token, node = Var name } env = 
    maybe (makeError token $ "Undefined identifier: " ++ name) 
            return $  Map.lookup name env 

typeCheck' Ast { token, node = RefAssignment ref value } env = do
    refT   <- typeCheck' ref env
    valueT <- typeCheck' value env
    case refT of
        RefType expectedT -> 
            when (valueT /= expectedT) $
                makeError token $ "Expected '" ++ show expectedT 
                    ++ "' type for the right-side value but found '" ++ show valueT ++ "' type."
        _ -> makeError token $ "Expected 'ref T' type as the left-side value but found '" 
                ++ show refT ++ "' type."
    return valueT

-- control flow
typeCheck' Ast { token, node = If { condition, body, elseBody } } env = do
    checkValue condition env BoolType
    bodyType <- typeCheck' body env
    maybe (return UnitType) (\ast -> do
        elseType <- typeCheck' ast env
        if elseType == bodyType then return bodyType
        else makeError token $ "Expected both branch of the if to produce same type but found '" 
                                ++ show bodyType ++ "' type and '" ++ show elseType ++ "' type."
     ) elseBody

typeCheck' Ast { node = Sequence fst snd } env  = do
    typeCheck' fst env
    typeCheck' snd env

typeCheck' Ast { node = While condition body } env  = do
    checkValue condition env BoolType
    typeCheck' body env
    return UnitType

-- Helper function c:
-- TODO: make check more generic so you can use it for this c:
checkBinary :: Ast -> TypeEnv -> Type -> Type -> Result Type
checkBinary Ast { token, node = Binary left _ right } env expected result = do
    left'  <- typeCheck' left  env
    right' <- typeCheck' right env

    if left' == right' 
        && right' == expected 
    then return result
    else makeError token $ "Expected two '" ++ show expected ++ "' type but found: '" 
                    ++ show left' ++ "' type and '" ++ show right' ++ "' type."

checkValue :: Ast -> TypeEnv -> Type -> Result Type -- TODO: finish this
checkValue ast env expected = do
    exprT <- typeCheck' ast env
    if exprT == expected 
        then return expected
    else makeError (token ast) $ "Expected a/an '" 
            ++ show expected ++ "' type but found a/an '" ++ show exprT ++ "' type."

checkEquals :: Ast -> TypeEnv -> Result Type
checkEquals Ast { token, node = Binary left _ right } env =  do
    left'  <- typeCheck' left env
    right' <- typeCheck' right env

    if left' == right' 
        then return BoolType
    -- TODO: think about the posibility of displaying the positions of the others types
    else makeError token $ "Expected two equal type but found: '" 
                              ++ show left' ++ "' type and '" ++ show right' ++ "' type."

makeError :: Token -> String -> Result a
makeError Token { Scanner.line, Scanner.position } msg = 
    Left $ Error { 
                   errType  = TypingError, 
                   message  = msg, 
            Errors.line     = line, 
            Errors.position = position 
        }

