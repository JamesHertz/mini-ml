{-# LANGUAGE RecordWildCards #-}
module Unification (
  Constraints,
  applySubstitution,
  replaceType,
  unify
) where

import Errors
import Types 

import UnionFind (TypedUnionFind)
import qualified UnionFind as Uf
import Data.Biapplicative (first)

import Data.Maybe (fromMaybe)
import Text.Printf (printf)

type ExpectedType = Type
type Constraints  = [(Type, ExpectedType, Token)]

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
      (x, y) | x == y -> unifyConstraints xs uf'' 
-- TODO: think about error messages
      (TypeVar x, TypeVar y) ->
          unifyConstraints xs (Uf.union x y uf'')

      (TypeVar x, y) | not $  t1' `appearsIn` [y] ->
          unifyConstraints xs (Uf.setReprType x y uf'')

      (x, TypeVar y) | not $ t2' `appearsIn` [x] ->
          unifyConstraints xs (Uf.setReprType y x uf'')

      -- TODO: handle token better
      (FuncType args1 ret1, FuncType args2 ret2) ->
        let 
          funcTypes1 = args1 ++ [ret1]
          funcTypes2 = args2 ++ [ret2]
          funcCons   = map (uncurry (,,token) ) $ matchTypes funcTypes1 funcTypes2
        in
          (`unifyConstraints` uf'') $  funcCons ++ xs

      (RefType x, RefType y) ->
        unifyConstraints ((x, y, token) : xs) uf''

      -- FIXME: do some special matches for better error messages
      _ -> makeError token $ printf "Couldn't match `%s` with `%s`" (show t1') (show t2')  
  where 
    appearsIn typ [] = False
    appearsIn typ (FuncType args ret : xs) = appearsIn typ (ret : args) || appearsIn typ xs
    appearsIn typ (RefType tp : xs)        = tp == typ || appearsIn typ xs
    appearsIn typ (x:xs)                   =  x == typ || appearsIn typ xs

    genFuncCons token x y = map (uncurry (,,token) ) $ matchTypes x y
    -- matchTypes [] []   = []
    matchTypes [x] [y] = [(x,y)]
    matchTypes [x] y   = [(x, FuncType (init y) (last y))]
    matchTypes  x [y]  = [(FuncType (init x) (last x), y)]
    matchTypes (x:xs) (y:ys) = (x,y) : matchTypes xs ys

updateType t@(TypeVar x) uf = maybe (t, Uf.newRepr x t uf) (,uf) $ Uf.lookupRepr x uf
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

updateType (FreeType _ tp) uf = updateType tp uf
updateType (RefType tp)    uf = first RefType $ updateType tp uf
updateType t uf = (t, uf)

{- TYPE SUBSTITUION -}
applySubstitution :: TypedAst -> TypedUnionFind -> TypedAst
applySubstitution Ast { ctx,  node = Binary left op right} uf = Ast {
    ctx = replaceType ctx uf, node = Binary (applySubstitution left uf) op (applySubstitution right uf)
  }

applySubstitution Ast { ctx,  node = Unary op value } uf = Ast {
    ctx = replaceType ctx uf, node = Unary op (applySubstitution value uf) 
  }

applySubstitution Ast { ctx,  node = FuncDecl args body } uf = Ast {
    ctx = replaceType ctx uf, node = FuncDecl (mapFunc <$> args ) (applySubstitution body uf) 
  }
  where 
    mapFunc (name, token, Just typ) =  (name, token, Just $ replaceType typ uf)

applySubstitution Ast { ctx,  node = Call func args } uf = 
  let 
    funcT   = applySubstitution func uf
    argsT   = (`applySubstitution` uf) <$> args
    (FuncType pars _) = type' funcT
    nodeBuilder = if length argsT == length pars then Call else PartialApplication
  in Ast {
      ctx  = replaceType ctx uf,
      node = nodeBuilder funcT argsT
    }

applySubstitution Ast { ctx,  node = PartialApplication func args } uf = Ast {
    ctx = replaceType ctx uf, node = PartialApplication (applySubstitution func uf) $ (`applySubstitution` uf) <$> args
  }

applySubstitution Ast { ctx,  node = LetBlock decls body } uf = Ast {
    ctx = replaceType ctx uf, node = LetBlock (mapFunc <$> decls) (applySubstitution body uf) 
  }
  where 
    mapFunc Assigment { varName, expectedType, assignValue } = Assigment { 
          assignValue = applySubstitution assignValue uf, ..
      }

applySubstitution Ast { ctx,  node = RefAssignment ref value } uf = Ast {
    ctx = replaceType ctx uf, node = RefAssignment (applySubstitution ref uf) (applySubstitution value uf)  
  }

applySubstitution Ast { ctx,  node = If {condition, body, elseBody} } uf = Ast {
    ctx = replaceType ctx uf, node = If { 
          condition = applySubstitution condition uf,
          body      = applySubstitution body uf,
          elseBody  =  (`applySubstitution` uf) <$> elseBody
      }
  }

applySubstitution Ast { ctx,  node = Sequence fst snd } uf = Ast {
    ctx = replaceType ctx uf, node = Sequence (applySubstitution fst uf) (applySubstitution snd uf)
  }

applySubstitution Ast { ctx,  node = While cond body } uf = Ast {
    ctx = replaceType ctx uf, node = While (applySubstitution cond uf) (applySubstitution body uf)
  }

applySubstitution Ast { ctx, node } uf = Ast { ctx = replaceType ctx uf, node }


replaceType :: Type -> TypedUnionFind -> Type
replaceType t@(TypeVar x)       uf = fromMaybe t $ Uf.lookupRepr x uf
replaceType (FuncType args ret) uf = ((`replaceType` uf) <$> args) `FuncType` replaceType ret uf
replaceType (FreeType _ tp)     uf = replaceType tp uf
replaceType (RefType tp)        uf = RefType $ replaceType tp uf
replaceType tp uf = tp

makeError :: Token -> String -> Result a
makeError Token { Types.line, Types.position } msg = 
    Left Error { 
                   errType  = TypingError, 
                   message  = msg, 
            Errors.line     = line, 
            Errors.position = position 
        }
