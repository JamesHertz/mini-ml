module TypeChecker where

data Type = IntType | BoolType | UnitType | RefType Type

instance Eq Type where
instance Show Type where
