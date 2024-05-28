module Unification where

import qualified Data.Map as Map
-- import qualified Types as Utils
import Control.Monad.State (State, get, put, evalState, gets, modify)
import Errors
import Types (Ast(..), AstNode(..), BasicAst, Token, TokenValue(..), token)

type GenId = String
data Type = 
      IntType 
    | BoolType 
    | UnitType 
    | RefType Type 
    | FuncType [Type] Type 
    | Generic GenId
    deriving (Eq)

-- type ExpectedType = Type
type Constraint   = (Type, Type, Token)
data TypeInfo = 
      Regular Type 
    | FreeType [GenId] Type

type TypeEnv = Map.Map String TypeInfo
type ConsState = State Int

calcConstraints :: BasicAst -> (Type, [Constraint])
calcConstraints Ast { node = Number _ } = (IntType, [])
calcConstraints Ast { node = Bool _ }   = (BoolType, [])
calcConstraints Ast { node = Unit }     = (UnitType, [])

-- arithmetic operations
calcConstraints ast@Ast { node = Binary _ PLUS  _ } = genArithConstraints ast
calcConstraints ast@Ast { node = Binary _ MINUS _ } = genArithConstraints ast
calcConstraints ast@Ast { node = Binary _ TIMES _ } = genArithConstraints ast
calcConstraints ast@Ast { node = Binary _ SLASH _ } = genArithConstraints ast

-- logical operations
calcConstraints ast@Ast { node = Binary _ AND _ }   = genLogicalConstraints ast
calcConstraints ast@Ast { node = Binary _ OR  _ }   = genLogicalConstraints ast

-- comparison operations
calcConstraints ast@Ast { node = Binary _  GT'  _ } = genComparisonConstraints ast
calcConstraints ast@Ast { node = Binary _  LT'  _ } = genComparisonConstraints ast
calcConstraints ast@Ast { node = Binary _ GT_EQ _ } = genComparisonConstraints ast
calcConstraints ast@Ast { node = Binary _ LT_EQ _ } = genComparisonConstraints ast

-- equality operations
calcConstraints ast@Ast { node = Binary _ EQ_EQ _ } = genEqualityConstraints ast
calcConstraints ast@Ast { node = Binary _ N_EQ  _ } = genEqualityConstraints ast

calcConstraints _ = (IntType, [])

genArithConstraints      ast = genBinaryConstraints ast IntType IntType IntType
genLogicalConstraints    ast = genBinaryConstraints ast BoolType BoolType BoolType
genComparisonConstraints ast = genBinaryConstraints ast IntType IntType BoolType

genEqualityConstraints Ast { node = Binary x _ y } = 
    let
        (t1, const1) = calcConstraints x
        (t2, const2) = calcConstraints y
    in ( BoolType, 
             const1 
          ++ const2 
          ++ [ (t1, t2, token x) ])

genBinaryConstraints :: BasicAst -> Type -> Type -> Type -> (Type, [Constraint])
genBinaryConstraints Ast { node = Binary x _ y } leftT rightT resultT = 
    let
        (t1, const1) = calcConstraints x
        (t2, const2) = calcConstraints y
    in 
        ( resultT, 
             const1 
          ++ const2 
          ++ [ (t1, leftT, token x), (t2, rightT, token y) ])

-- genConstraints x y left





