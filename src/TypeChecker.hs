-- {-# LANGUAGE RecordWildCards #-}

module TypeChecker (
    Type(..),
    typeCheck,
    TypedAst
) where

import Types
import Errors

import Data.Set (fromList, toList)
import qualified UnionFind as Uf
import qualified Data.Set  as Set
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Except (throwE)
import Data.Maybe (fromJust)
import Text.Printf (printf)
import Control.Monad (foldM, join)
import Data.Tuple.Extra (thd3)

import Unification (Constraints, applySubstitution, replaceType, unify)

import qualified Data.Map as Map
import Control.Monad.State (State, runState, get, modify)

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
           funcCons  
        ++ (type' funcAst, FuncType (type' <$> argsAst) (TypeVar retType), token func) : argsCons 
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
              return $ Uf.newRepr free (TypeVar new) uf
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
deduplicate = toList . fromList
