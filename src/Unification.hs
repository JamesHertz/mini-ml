module Unification where

import qualified Data.Map as Map
-- import qualified Types as Utils
import Control.Monad.State (State, get, put, evalState, gets, modify, runState)
import Errors
import Types (
  Ast(..), AstNode(..), BasicAst, Token(..),  TypedAst,
  TokenValue(..),  Type(..), GenId, type',
  token
  )
import Distribution.Compat.CharParsing (letter)

import UnionFind (TypedUnionFind, newRepr, setReprType, union)
import qualified UnionFind as Uf
import Data.Biapplicative (first, Bifunctor (bimap))
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Except (throwE)
import Data.Maybe (fromMaybe, isNothing)
import Text.Printf (printf)
import Control.Monad (foldM, join)

type ExpectedType = Type
type Constraints  = [(Type, ExpectedType, Token)]

type TypeEnv   = Map.Map String Type
-- type Context   = Int
type ConsState = ExceptT Error (State Int) 

typeCheck :: BasicAst -> Result TypedAst
typeCheck ast = do 
    let 
       (value, state)  =  runState (runExceptT $ calcConstraints ast Map.empty) 0
    (ast', constrs) <- value
    subs   <- unify constrs 
    return $ applySubstitution ast' subs 


      
applySubstitution :: TypedAst -> TypedUnionFind -> TypedAst
applySubstitution ast _ = ast

calcConstraints :: BasicAst -> TypeEnv -> ConsState (TypedAst, Constraints)
calcConstraints Ast { node = Number x } env = return' Ast { ctx = IntType,  node = Number x }
calcConstraints Ast { node = Bool x }   env = return' Ast { ctx = BoolType, node = Bool x   }
calcConstraints Ast { node = Unit }     env = return' Ast { ctx = UnitType, node = Unit }

calcConstraints ast@Ast { node = Binary _ PLUS  _ } env = genArithConstraints ast env
calcConstraints ast@Ast { node = Binary _ MINUS _ } env = genArithConstraints ast env
calcConstraints ast@Ast { node = Binary _ TIMES _ } env = genArithConstraints ast env
calcConstraints ast@Ast { node = Binary _ SLASH _ } env = genArithConstraints ast env

calcConstraints ast@Ast { node = Binary _ LT' _ }   env = genComparisonConstraints ast env 
calcConstraints ast@Ast { node = Binary _ GT' _ }   env = genComparisonConstraints ast env
calcConstraints ast@Ast { node = Binary _ LT_EQ _ } env = genComparisonConstraints ast env
calcConstraints ast@Ast { node = Binary _ GT_EQ _ } env = genComparisonConstraints ast env

calcConstraints ast@Ast { node = Binary _ AND _ } env = genLogicalConstraints ast env
calcConstraints ast@Ast { node = Binary _ OR  _ } env = genLogicalConstraints ast env

calcConstraints ast@Ast { node = Binary _ EQ_EQ _ } env = genEqualityConstraints ast env
calcConstraints ast@Ast { node = Binary _ N_EQ  _ } env = genEqualityConstraints ast env

calcConstraints ast@Ast { node = Call func args } env = do 
    funcC <- calcConstraints func env
    argsC <- mapM (`calcConstraints` env) args

    let 
      (funcAst, funcCons) = funcC
      argsAst  = fst <$> argsC
      argsCons = argsC >>= snd

    newFuncAst <- case type' funcAst of 
                      FreeType freeVars (FuncType args ret) -> do
                        map <- foldM (\ m tp ->  do
                                new <- freshVar
                                return $ Map.insert (TypeVar tp) (TypeVar new) m
                              ) Map.empty freeVars
                        let newType = FuncType (replaceType map <$> args) $ replaceType map ret
                        return Ast { ctx = newType, node = node funcAst }
                      tp   -> return funcAst
    retType <- freshVar
    return (
        Ast { ctx = TypeVar retType, node = Call newFuncAst argsAst } , 
        (type' funcAst, FuncType (type' <$> argsAst) (TypeVar retType), token func) : argsCons
        )
    where 
      replaceType m t = fromMaybe t $ Map.lookup t m

calcConstraints ast@Ast { node = Var name } env = 
  case Map.lookup name env of 
      Nothing  -> makeError (token ast) $ printf "Undefined identifier: '%s'" name
      Just ti  -> return (Ast { ctx = ti, node = Var name }, [])

-- FIXME: refactor to function to in Error.hs
makeError :: Token -> String -> ConsState a
makeError Token { Types.line, Types.position } msg = 
    throwE Error { 
                   errType  = TypingError, 
                   message  = msg, 
            Errors.line     = line, 
            Errors.position = position 
        }

freshVar :: ConsState GenId
freshVar  = do 
    idx <- get 
    let var = "x" ++ show idx
    modify  (+1) 
    return var

-- helper functions c:
genArithConstraints      ast env = genBinaryConstraints ast env IntType IntType
genLogicalConstraints    ast env = genBinaryConstraints ast env BoolType BoolType
genComparisonConstraints ast env = genBinaryConstraints ast env IntType BoolType

genBinaryConstraints :: BasicAst -> TypeEnv -> Type -> Type -> ConsState (TypedAst, Constraints)
genBinaryConstraints ast@Ast { node = Binary left op right } env expected result = do
    leftC  <- calcConstraints left  env
    rightC <- calcConstraints right env

    let
        (leftAst, leftCons)   = leftC
        (rightAst, rightCons) = rightC

    return (
        Ast { ctx = result, node = Binary leftAst op leftAst },
        [
            (type' leftAst, expected,  token left),
            (type' rightAst, expected, token right)
        ]
     )

genEqualityConstraints ast@Ast { node = Binary left op right } env = do
    leftC  <- calcConstraints left  env
    rightC <- calcConstraints right env

    let
        (leftAst, leftCons)   = leftC
        (rightAst, rightCons) = rightC

    return (
        Ast { ctx = BoolType , node = Binary leftAst op leftAst },
        [
            (type' rightAst, type' leftAst,  token right)
        ]
     )

return' value = return (value, [])


{- UNIFICATION -}
unify :: Constraints -> Result TypedUnionFind
unify = (`unifyConstraints` Uf.empty)

unifyConstraints :: Constraints -> TypedUnionFind -> Result TypedUnionFind
unifyConstraints [] uf = return uf

-- TODO: try to fix this
unifyConstraints ((t1, t2, token):xs) uf = 
  let 
     (t1', uf')  = updateType t1 uf
     (t2', uf'') = updateType t2 uf'
  in case (t1', t2') of 
      -- TODO: refactor this
      (TypeVar x, FuncType args ret) | t1' `appearsIn` [t2'] ->
          makeError' token $ printf "Couldn't match '%s' with '%s'" (show t1') (show t2')

      (FuncType args ret, TypeVar x) | t2' `appearsIn` [t1'] ->
          makeError' token $ printf "Couldn't match '%s' with '%s'" (show t1') (show t2')

      (TypeVar x, TypeVar y) ->
          unifyConstraints xs (union x y uf'')

      (TypeVar x, y) ->
          unifyConstraints xs (setReprType x y uf'')

      (x, TypeVar y) ->
          unifyConstraints xs (setReprType y x uf'')

      -- TODO: look at this c:
      (FuncType args1 ret1, FuncType args2 ret2) ->
        unifyConstraints (zipWith (,,token) (ret1 : args1) (ret2 : args2) ++ xs) uf'' 

      (x, y) -> 
        if x == y then 
          unifyConstraints xs uf'' 
        else 
          makeError' token $ printf "Couldn't match '%s' with '%s'" (show x) (show y)
  where 
    appearsIn typ [] = False
    appearsIn typ (FuncType args ret : xs) = appearsIn typ (ret : args) || appearsIn typ xs
    appearsIn typ (x:xs) = x == typ || appearsIn typ xs

    

  -- if (TypeVar x) `elem` ret : args then 
  --     makeError token $ printf "Couldn't match '%s' with '%s'" (TypeVar x) (FuncType args ret)
  -- else 
  --     return unifyConstraints ()

updateType t@(TypeVar x) uf = maybe (t, newRepr x t uf) (,uf) $ Uf.lookupRepr x uf
updateType (FuncType args ret) uf = 
  let 
      (updatedTypes, uf') = foldr (\tp (arr, uf) ->
            let 
              (updatedType, uf') = updateType tp uf 
            in 
              (updatedType : arr, uf')
        ) ([], uf) $ ret : args
      ret':args' = updatedTypes
    in 
      (FuncType args' ret', uf')
updateType (FreeType _ tp) uf = (tp, uf)
updateType t uf = (t, uf)


makeError' :: Token -> String -> Result a
makeError' Token { Types.line, Types.position } msg = 
    Left Error { 
                   errType  = TypingError, 
                   message  = msg, 
            Errors.line     = line, 
            Errors.position = position 
        }
