module TypeChecker (
    Type(..),
    typeCheck
) where

import Parser

data Type = IntType | BoolType deriving (Eq, Show)

typeCheck :: Ast -> Type
typeCheck (Number x) = IntType
typeCheck (Bool x)   = BoolType
typeCheck (Add left right)  = check (typeCheck left) (typeCheck right) IntType
typeCheck (Sub left right)  = check (typeCheck left) (typeCheck right) IntType
typeCheck (Div left right)  = check (typeCheck left) (typeCheck right) IntType
typeCheck (Mult left right) = check (typeCheck left) (typeCheck right) IntType
typeCheck (And left right)  = check (typeCheck left) (typeCheck right) BoolType
typeCheck (Or left right)   = check (typeCheck left) (typeCheck right) BoolType

-- TODO: improve this c:
check :: Type -> Type -> Type -> Type
check fst snd expected = 
    if fst == snd && snd == expected then
        expected
    else
        error "Type error c:"