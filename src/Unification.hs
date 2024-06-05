{-# LANGUAGE RecordWildCards #-}
module Unification where



import qualified Data.Map as Map
-- import qualified Types as Utils
import Control.Monad.State (State, get, put, evalState, gets, modify, runState)
import Errors
import Types (
  Ast(..), AstNode(..), BasicAst, Token(..),  TypedAst,
  TokenValue(..),  Type(..), GenId, type',
  token, Assigment (Assigment, assignValue, expectedType, varName)
  )

import Data.Set (fromList, toList)
import UnionFind (TypedUnionFind, newRepr, setReprType, union)
import qualified UnionFind as Uf
import qualified Data.Set as Set
import Data.Biapplicative (first, Bifunctor (bimap))
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Except (throwE)
import Data.Maybe (fromMaybe, isNothing, fromJust)
import Text.Printf (printf)
import Control.Monad (foldM, join)
import Control.Comonad.Env (env)
import Data.Tuple.Extra (thd3)

type ExpectedType = Type
type Constraints  = [(Type, ExpectedType, Token)]

type TypeEnv   = Map.Map String Type
type ConsState = ExceptT Error (State Int) 

typeCheck :: BasicAst -> Result TypedAst
typeCheck ast = do 
    let 
       (value, state)  =  runState (runExceptT $ calcConstraints ast Map.empty) 0
    (ast', constrs) <- value
    subs   <- unify constrs 
    return $ applySubstitution ast' subs 

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
calcConstraints ast@Ast { node = Binary _  N_EQ _ } env = genEqualityConstraints ast env

calcConstraints ast@Ast { node = Unary op value } env = do 
  (valueAst, cons) <- calcConstraints value env
  (retType, unaryCons) <- case op of 
                              MINUS   -> return (IntType,  [(type' valueAst, IntType, token value)])
                              NOT     -> return (BoolType, [(type' valueAst, BoolType, token value)])
                              PRINTLN -> return (UnitType, [])
                              PRINT   -> return (UnitType, [])
                              NEW     -> return (RefType $ type' valueAst, [])
                              BANG    -> do 
                                var <- freshVar
                                return (TypeVar var, [(type' valueAst, RefType $ TypeVar var, token value)])

  return (Ast { ctx = retType, node = Unary op valueAst }, cons ++ unaryCons)

-- control flow
calcConstraints ast@Ast { node = If { condition, body, elseBody } } env = do 
  (condAst, condCons) <- calcConstraints condition env
  (bodyAst, bodyCons) <- calcConstraints body env
  (elseAst, elseCons, retType) <- maybe (return (Nothing, [], UnitType)) (\ast -> do 
    (elseAst, elseCons) <- calcConstraints ast env
    return (
        Just elseAst, 
        elseCons ++ [(type' elseAst, type' bodyAst, token ast)],
        type' bodyAst
      )
   ) elseBody
  
  return (Ast { ctx = retType, node = If condAst bodyAst elseAst }, condCons ++ bodyCons ++ elseCons)

calcConstraints ast@Ast { node = While cond body } env = do 
  (condAst, condCons) <- calcConstraints cond env
  (bodyAst, bodyCons) <- calcConstraints body env
  return (
    Ast { ctx = UnitType, node = While condAst bodyAst},
       condCons 
    ++ bodyCons
    ++ [ (type' condAst, BoolType, token cond) ]
   )

calcConstraints ast@Ast { node = Sequence fst snd } env = do 
  (fstAst, fstCons) <- calcConstraints fst env
  (sndAst, sndCons) <- calcConstraints snd env
  return ( Ast { ctx = type' sndAst, node = Sequence fstAst sndAst }, fstCons ++ sndCons )

-- functions stuffs 
calcConstraints ast@Ast { node = FuncDecl pars body } env = do 
  (newPars, _ ) <- foldM (\(arr, set) (name, token, parType) -> 
      if Set.member name set then 
         makeError token $ printf "Parameter name '%s' already used" name
      else do 
        typ <- case parType of 
                Just t  -> return t 
                Nothing -> TypeVar <$> freshVar 
        return ((name, token, Just typ) : arr, Set.insert name set)
    ) ([], Set.empty) pars

  let 
    newPars' = reverse newPars
    newEnv   = foldl (\ env (name, _, Just parType) -> Map.insert name parType env ) env newPars'
  (bodyAst, bodyCons) <- calcConstraints body newEnv

  return (Ast { 
      ctx = FuncType (fromJust . thd3 <$> newPars') (type' bodyAst ), 
      node = FuncDecl newPars' bodyAst }, 
      bodyCons 
    )

calcConstraints ast@Ast { node = Call func args } env = do 
    (funcAst, funcCons) <- calcConstraints func env
    argsC <- mapM (`calcConstraints` env) args

    let 
      argsAst  = fst <$> argsC
      argsCons = argsC >>= snd

    retType <- freshVar
    return (
        Ast { ctx = TypeVar retType, node = Call funcAst argsAst } , 
        (type' funcAst, FuncType (type' <$> argsAst) (TypeVar retType), token func) : argsCons
     )

-- identifiers & ref
calcConstraints ast@Ast { node = LetBlock assigns body } env = do
   (newEnv, assignsInfo) <- foldM mapFunc (env,[]) assigns
   let 
      newAssigns =  fst <$> reverse assignsInfo
      cons       =  snd <$> reverse assignsInfo
   (bodyAst, bodyCons) <- calcConstraints body newEnv
   return (Ast {
        ctx  = type' bodyAst,
        node = LetBlock newAssigns bodyAst
     }, join cons ++ bodyCons )
 where 
   -- mapFunc :: (TypeEnv, [(Assigment Type, Constraints)]) -> Assigment Token -> ConsState (TypeEnv, [(Assigment Type, Constraints)]) 
   mapFunc (env, arr) Assigment { 
         varName, assignValue = func@(Ast { node = FuncDecl _ _ }),  expectedType 
        } = do
      var <- freshVar
      (funcAst, funcCons) <- calcConstraints func $ Map.insert varName (TypeVar var) env
      let
          funcCons'   = maybe funcCons (\(typ, token) -> funcCons ++ [(TypeVar var, typ, token)] ) expectedType
          -- FIXME: a way to get a token for this ...
          unifyResult = unify $ (TypeVar var, type' funcAst, token func) : funcCons'
      case unifyResult of
          Left err -> throwE err
          _ -> return ()

      let 
        Right uf  = unifyResult
        funcBody  = applySubstitution funcAst uf
        funcType  = replaceType (type' funcAst) uf
        freeVars  = deduplicate . filter (olderThan var) $ typeVarsOf funcType
        transCons = (\(f,s,t) -> (replaceType f uf, replaceType s uf, t)) <$> funcCons
        cons      = filter (\(fst, snd, _ ) -> 
            not . any (olderThan var) $ typeVarsOf fst ++ typeVarsOf snd 
          ) transCons
        valueType = FreeType freeVars funcType
      return (
          Map.insert varName valueType env,
          ( Assigment { 
              varName, 
              expectedType = Nothing, -- no need anymore
              assignValue  = Ast { ctx = valueType, node = node funcBody }
            }, cons) : arr 
        )
       
   mapFunc (env, arr) Assigment { varName, assignValue,  expectedType } = do
      (valueAst, valueCons) <- calcConstraints assignValue env
      return (
          Map.insert varName (type' valueAst) env,
           ( 
              Assigment { varName, expectedType = Nothing, assignValue = valueAst },
              maybe valueCons (\(typ, token) -> (type' valueAst, typ, token) : valueCons) expectedType
           )  : arr
       )
      
   olderThan y x = length x > length y || length x == length y && x >= y
   typeVarsOf (FuncType args ret) = (args >>= typeVarsOf) ++ typeVarsOf ret
   typeVarsOf (TypeVar x) = [x]
   typeVarsOf (RefType x) = typeVarsOf x
   typeVarsOf typ = []

calcConstraints ast@Ast { node = Var name } env = 
  -- TODO: instanciate free vars ...
  case Map.lookup name env of 
      Nothing  -> makeError (token ast) $ printf "Undefined identifier: '%s'" name
      Just (FreeType freeVars func) -> do
        uf <- foldM (\ uf free ->  do
              -- TODO: general local and global vars c:
              new <- freshVar
              return $ newRepr free (TypeVar new) uf
            ) Uf.empty freeVars
        let newType = replaceType func uf
        return (Ast { ctx = newType, node = Var name }, [])
      Just ti  -> return (Ast { ctx = ti, node = Var name }, [])

calcConstraints ast@Ast { node = RefAssignment ref value } env = do
  (refAst, refCons)     <- calcConstraints ref env
  (valueAst, valueCons) <- calcConstraints value env
  var <- freshVar
  return (
    Ast { ctx = TypeVar var, node = RefAssignment refAst valueAst },
       refCons
    ++ valueCons
    ++ [
      (type' valueAst, TypeVar var, token value),
      (type' refAst, RefType $ TypeVar var, token ref)
    ]
   )

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
    let var = "g" ++ show idx
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
      (x, y) | x == y -> unifyConstraints xs uf'' 
-- TODO: think about error messages
      (TypeVar x, TypeVar y) ->
          unifyConstraints xs (union x y uf'')

      (TypeVar x, y) | not $  t1' `appearsIn` [y] ->
          unifyConstraints xs (setReprType x y uf'')

      (x, TypeVar y) | not $ t2' `appearsIn` [x] ->
          unifyConstraints xs (setReprType y x uf'')

      -- TODO: handle token better
      (FuncType args1 ret1, FuncType args2 ret2) | length args1 == length args2 ->
        unifyConstraints (zipWith (,,token) (ret1 : args1) (ret2 : args2) ++ xs) uf'' 

      (RefType x, RefType y) ->
        unifyConstraints ((x, y, token) : xs) uf''

      -- FIXME: do some special matches for better error messages
      _ -> makeError' token $ printf "Couldn't match `%s` with `%s`" (show t1') (show t2')  
  where 
    appearsIn typ [] = False
    appearsIn typ (FuncType args ret : xs) = appearsIn typ (ret : args) || appearsIn typ xs
    appearsIn typ (RefType tp : xs)        = tp == typ || appearsIn typ xs
    appearsIn typ (x:xs)                   =  x == typ || appearsIn typ xs

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

applySubstitution Ast { ctx,  node = Call func args } uf = Ast {
    ctx = replaceType ctx uf, node = Call (applySubstitution func uf) $ (`applySubstitution` uf) <$> args
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

makeError' :: Token -> String -> Result a
makeError' Token { Types.line, Types.position } msg = 
    Left Error { 
                   errType  = TypingError, 
                   message  = msg, 
            Errors.line     = line, 
            Errors.position = position 
        }

deduplicate = toList . fromList
