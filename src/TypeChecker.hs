-- {-# LANGUAGE RecordWildCards #-}

module TypeChecker (
    Type(..),
    typeCheck,
    TypedAst
) where



-- TODO: improve overall error reporting c:
import Parser ( Ast(..), AstNode(..), Token(..), Assigment(..), BasicAst)
import Scanner (Token(..), TokenValue(..))
import qualified Data.Map as Map
import Control.Monad (foldM, when)
import Errors  
import Data.Profunctor.Closed (Environment(Environment))
import Text.Printf (printf)

data Type = IntType | BoolType | UnitType | RefType Type deriving (Eq)

type TypeEnv = Map.Map String Type

instance Show Type where
    show IntType       = "int"
    show BoolType      = "bool"
    show UnitType      = "unit"
    show (RefType typ) = "ref " ++ show typ


type TypedAst = Ast Type

-- TODO: add to Types.hs
token :: BasicAst -> Token
token = ctx

type' :: TypedAst -> Type
type' = ctx


-- 
typeCheck :: BasicAst -> Result TypedAst
typeCheck ast = typeCheck' ast Map.empty

typeCheck' :: BasicAst -> TypeEnv -> Result TypedAst
typeCheck' Ast { node = Number x } env = return $ Ast IntType  (Number x)
typeCheck' Ast { node = Bool x }   env = return $ Ast BoolType (Bool x)
typeCheck' Ast { node = Unit }     env = return $ Ast UnitType Unit

-- unaries
typeCheck' Ast { node = Unary NEW value } env = do 
    valueT <- typeCheck' value env
    return Ast { 
        ctx = RefType (type' valueT),
        node = Unary NEW valueT
    }

typeCheck' ast@Ast { node = Unary BANG value } env = do
    valueT <- typeCheck' value env
    case type' valueT of
        RefType t -> return Ast { 
                ctx  = type' valueT,
                node = Unary BANG valueT
            }
        _ -> makeError (token ast) $ 
                printf "Expected 'ref T' type but found '%s' type." (show $ type' valueT)

typeCheck' Ast { node = Unary op value  } env = 
    let (valueT, typ) =
          case op of 
              MINUS   -> (checkValue value env IntType, IntType)
              NOT     -> (checkValue value env BoolType, BoolType)
              PRINTLN -> (typeCheck' value env, UnitType)
              PRINT   -> (typeCheck' value env, UnitType)
    in Ast typ . Unary op <$> valueT

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
--
typeCheck' ast@Ast { node = Binary _ EQ_EQ _ } env = checkEquals ast env
typeCheck' ast@Ast { node = Binary _ N_EQ  _ } env = checkEquals ast env

-- handling identifier c:
-- TODO: look for a better solution
typeCheck' Ast { node = LetBlock assigns body } env = do
    result <- foldM mapFunc (env,[]) assigns
    bodyT  <- typeCheck' body (fst result)
    return Ast { ctx = type' bodyT, node = LetBlock (reverse $ snd result) bodyT } 
    where 
        mapFunc (env, assingsT) Assigment { varName, assignValue, expectedType } = do 
            valueT <- typeCheck' assignValue env

            let 
                valueType = type' valueT
                assingsT' = Assigment varName Nothing valueT : assingsT
                newEnv    = Map.insert varName valueType env
                result    = (newEnv, assingsT')

            maybe (return result) (\(typ, token) -> 
                if valueType == typ then 
                    return result
                else makeError token $ 
                        printf "Expected '%s' type, but expression produced '%s' type."
                                (show valueType) (show typ)
                ) expectedType

typeCheck' ast@Ast { node = Var name } env = 
    maybe (makeError (token ast) $ "Undefined identifier: " ++ name)
            (return . (`Ast` Var name)) $  Map.lookup name env 

typeCheck' ast@Ast { node = RefAssignment ref value } env = do
    refT   <- typeCheck' ref env
    valueT <- typeCheck' value env

    let
         refType = type' refT
         valueType = type' valueT

    case refType of
        RefType expectedType -> 
            when (valueType /= expectedType) $
                makeError (token ast) $ 
                  printf "Expected '%s' type for the right-side value but found '%s' type."
                         (show expectedType) (show valueType)
        _ -> makeError (token ast) $ 
                printf "Expected 'ref T' type as the left-side value but found '%s' type." (show refType)
    return Ast { 
        ctx  = valueType,
        node = RefAssignment refT valueT 
    }

-- control flow
typeCheck' ast@Ast { node = If { condition, body, elseBody } } env = do
    condT <- checkValue condition env BoolType
    bodyT <- typeCheck' body env
    let makeAst typ elseT = return Ast {
             ctx  = typ, 
             node = If condT bodyT elseT
         }
    maybe (makeAst UnitType Nothing) (\ast -> do
        elseT <- typeCheck' ast env
        let bodyType = type' bodyT
            elseType = type' elseT
        if elseType == elseType 
            then makeAst bodyType (Just elseT)
        else makeError (token ast) $ 
            printf "Expected both branch of the if to produce same type but found '%s' type and '%s' type." 
                   (show bodyType) (show elseType)
      ) elseBody

typeCheck' Ast { node = Sequence fst snd } env  = do
    fstT <- typeCheck' fst env
    sndT <- typeCheck' snd env
    return Ast { ctx = type' sndT, node = Sequence fstT sndT }

typeCheck' Ast { node = While condition body } env = do
    condT <- checkValue condition env BoolType
    bodyT <- typeCheck' body env
    return Ast { ctx = UnitType, node = While condT bodyT }

-- Helper function c:
-- TODO: make check more generic so you can use it for this c:
checkBinary :: BasicAst -> TypeEnv -> Type -> Type -> Result TypedAst
checkBinary ast@Ast { node = Binary left op right } env expected result = do
    leftT  <- typeCheck' left  env
    rightT <- typeCheck' right env

    let
        leftType  = type' leftT
        rightType = type' rightT

    if leftType == rightType
        && rightType == expected 
    then return Ast { ctx = expected, node = Binary leftT op rightT }
    else makeError (token ast) $ 
          printf "Expected two '%s' type but found '%s' type and '%s' type." 
                 (show expected) (show leftType) (show rightType)

-- TODO: finish this
checkValue :: BasicAst -> TypeEnv -> Type -> Result TypedAst 
checkValue ast env expected = do
    exprT <- typeCheck' ast env
    if ctx exprT == expected 
        then return exprT
    else makeError (token ast) $ 
          printf "Expected a/an '%s' type but found a/an '%s' type." 
                 (show expected) (show exprT)

checkEquals :: BasicAst -> TypeEnv -> Result TypedAst
checkEquals ast@Ast { node = Binary left op right } env =  do
    leftT  <- typeCheck' left env
    rightT <- typeCheck' right env

    let
        leftType  = type' leftT
        rightType = type' rightT

    if  leftType == rightType
        then return Ast { ctx = BoolType, node = Binary leftT op rightT }
    -- TODO: think about the posibility of displaying the positions of the others types
    else makeError (token ast) $ 
          printf "Expected two equal type but found: '%s' type and '%s' type." 
                 (show leftType) (show rightType)

makeError :: Token -> String -> Result a
makeError Token { Scanner.line, Scanner.position } msg = 
    Left $ Error { 
                   errType  = TypingError, 
                   message  = msg, 
            Errors.line     = line, 
            Errors.position = position 
        }

