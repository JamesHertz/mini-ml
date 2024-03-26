module TypeChecker (
    Type(..),
    typeCheck
) where

-- TODO: improve overall error reporting c:
import Parser ( Ast(..) )

data Type = IntType | BoolType deriving (Eq)

instance Show Type where
    show IntType  = "integer"
    show BoolType = "boolean"

-- instance Show Type 
typeCheck :: Ast -> Type
typeCheck (Number x) = IntType
typeCheck (Bool x)   = BoolType
typeCheck (Add left right)  = check (typeCheck left) (typeCheck right) IntType
typeCheck (Sub left right)  = check (typeCheck left) (typeCheck right) IntType
typeCheck (Div left right)  = check (typeCheck left) (typeCheck right) IntType
typeCheck (Mult left right) = check (typeCheck left) (typeCheck right) IntType
typeCheck (And left right)  = check (typeCheck left) (typeCheck right) BoolType
typeCheck (Or left right)   = check (typeCheck left) (typeCheck right) BoolType
typeCheck (Neg x) = 
    let 
        tt = typeCheck x
    in if tt == IntType then IntType
       else error $ "Type error: Expected an integer but found a " ++ show tt

typeCheck (Equals left right)    =  checkEquals (typeCheck left) (typeCheck right)
typeCheck (NotEquals left right) =  checkEquals (typeCheck left) (typeCheck right)

typeCheck (GreaterThan   left right) = checkComparison (typeCheck left) (typeCheck right)
typeCheck (LessThan      left right) = checkComparison (typeCheck left) (typeCheck right)
typeCheck (GreaterThanEq left right) = checkComparison (typeCheck left) (typeCheck right)
typeCheck (LessThanEq    left right) = checkComparison (typeCheck left) (typeCheck right)

typeCheck (Not expr) = 
    let 
        exprT =  typeCheck expr
    in 
        if exprT == BoolType then BoolType
        else error $ "Expected a boolean but got a " ++ show exprT

-- TODO: improve this c:
check :: Type -> Type -> Type -> Type
check fst snd expected = 
    if fst == snd && snd == expected then
        expected
    else
        error $ "Type error: Expected two " ++ show expected 
                 ++ " but found " ++ show fst ++ " and " ++ show snd

checkComparison :: Type -> Type -> Type
checkComparison fst snd = 
    if fst == snd && snd == IntType then
        BoolType
    else
        error $ "Type error: Expected two integers but found: " ++ show fst ++ " and " ++ show snd

checkEquals :: Type -> Type -> Type
checkEquals fst snd = 
    if fst == snd then BoolType
    else error $ "Type error: Expected two booleans or two integers but found: " 
                      ++ show fst ++ " and " ++ show snd 