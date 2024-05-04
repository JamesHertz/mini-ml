{-# LANGUAGE RecordWildCards #-}
module Compiler(
    Instr(..),
    compile,
    Program,
    Label,
    Cond(..)
) where

import Types 
import Control.Monad.State (State, evalState, gets, put, modify, foldM)
import Errors (Result)
import Data.Char (toLower)
import Text.Printf (printf)

import qualified Data.Map as Map

type JvmProgram = ([Instr], [ClassFile])
data JvmType    = JvmInt | JvmBool | CustomUnit | Class String
-- TODO: 
--       x  add the remaning types
--       x  handle refs types
instance Show JvmType where
    show JvmInt  = "I"
    show JvmBool = "Z"
    show CustomUnit = "stdlib/Unit"
    show (Class name) = name

type ClassField = (String, JvmType)
data ClassFile = ClassFile {
      name   :: String,
      fields :: [ClassField]
} deriving Show

-- helper types
type Label = String
type FieldSpec  = String
type ClassSpec  = String
type MethodSpec = String
type TypeSpec   = String

data Instr = 
      IAdd
    | ISub
    | IDiv
    | IMul
    | IAnd
    | IOr
    | INeg
    | Goto Label
    | SIpush Int
    | IfIcomp Cond Label
    | Iconst_1
    | Iconst_0
    | ILabel Label
    | Print Type 

    | Iload Int
    | Aload Int

    | Istore Int
    | Astore Int
    | Ifeq Label -- same as if zero c:
    
    | New ClassSpec
    | InvokeSpecial MethodSpec

    | Putfield FieldSpec JvmType
    | GetField FieldSpec JvmType

    | GetStatic FieldSpec JvmType
    | Invoke       MethodSpec
    | InvokeStatic MethodSpec
    | Dup
    | Pop
    -- | Nop -- FIXME: find a way around this

 deriving Show

data Cond  = CEQ | CNE | CLT | CGT | CLE | CGE
instance Show Cond where
  show CEQ = "eq"
  show CNE = "ne"
  show CLT = "lt"
  show CGT = "gt "
  show CLE = "le"
  show CGE = "ge"

-- some instructions alias
pushUnit :: Instr
pushUnit = GetStatic "stdlib/Unit/SINGLE" CustomUnit

ifFalse :: String -> Instr
ifFalse = Ifeq

-- data FrameId = String
data Context = Context {
    labelCount   :: Int,
    frameIdCount :: Int,
    depth        :: Int,
    classFiles   :: [ClassFile], -- TODO: think about using a map to solve the refs problem
    lastFrame    :: Maybe String
 }

data VarLoc  = VarLoc {
        envDepth  :: Int,
        frameLoc  :: String,
        varType   :: JvmType
    } deriving Show

type CompilerEnv   = Map.Map String VarLoc
type CompilerState = State Context
type Program = [Instr]

-- TODO: think about this c:
compile :: TypedAst -> Result Program
compile ast = return . evalState (compile' ast Map.empty) $ Context {
    labelCount   = 0,
    frameIdCount = 0,
    depth        = 0,
    classFiles   = [],
    lastFrame    = Nothing
 }

compile' :: TypedAst -> CompilerEnv -> CompilerState Program
compile' Ast { node = Number n }   env = return [SIpush n]
compile' Ast { node = Bool value } env = return [SIpush $ if value then 1 else 0] 
compile' Ast { node = Unit }       env = return [pushUnit]

compile' Ast { node = Binary left op right } env = do
  left'  <- compile' left  env
  right' <- compile' right env
  instr  <- case op of
      PLUS  -> return [IAdd]
      MINUS -> return [ISub]
      TIMES -> return [IMul]
      SLASH -> return [IDiv]
      OR    -> return [IOr]
      AND   -> return [IAnd]
      EQ_EQ -> compileCompOperations CEQ
      N_EQ  -> compileCompOperations CNE
      LT'   -> compileCompOperations CLT
      GT'   -> compileCompOperations CGT
      LT_EQ -> compileCompOperations CLE
      GT_EQ -> compileCompOperations CGE
  return $ left' ++ right' ++ instr

-- TODO: add an Unit type
compile' Ast { node = Unary op value } env = do
    value' <- compile' value env
    let instr = case op of
         NOT   -> [INeg] -- FIXME: use if and iconsts c: (someday)
         MINUS -> [INeg]
         PRINTLN -> compilePrints op (type' value)
         PRINT   -> compilePrints op (type' value)

    return $ value' ++ instr

compile' Ast { node = Sequence fst snd } env = do
    fst' <- compile' fst env
    snd' <- compile' snd env
    return $ fst' ++ [ Pop ] ++ snd'

compile' Ast { node = If { condition, body, elseBody } } env = do
    condInstrs <- compile' condition env
    bodyInstrs <- compile' body env
    l1         <- genLabel
    case elseBody of 
        Just elseBody' -> do
          elseInstrs <- compile' elseBody' env
          l2         <- genLabel
          return $    condInstrs  ++ [ifFalse l1]
                   ++ bodyInstrs  ++ [Goto l2]
                   ++ [ILabel l1] ++ elseInstrs
                   ++ [ILabel l2]
        Nothing -> return $    condInstrs ++ [ifFalse l1] 
                            ++ bodyInstrs ++ [
                             Pop, ILabel l1, pushUnit
                           ]

compile' Ast { node = LetBlock assigns body } env = do
    currDepth <- currEnvDepth
    frameId   <- genFrameId
    result    <- foldM mapFunc (
            env, [], 0 :: Int, [ ]
        ) assigns

    prefFrame <- gets lastFrame
    let 
        (newEnv, fields, _, varInstrs) = result
        fields' = maybe fields (
             \prefId -> ("SL", Class prefId) : fields
            ) prefFrame
        frameInit  = [ 
            New frameId, 
            InvokeSpecial $ printf "%s/<init>()V" frameId 
         ]
    
    modify $ \Context{..} -> Context{ 
            depth = currDepth + 1, 
            lastFrame = Just frameId, 
            classFiles = ClassFile {
                name   = printf "Frame_%d" frameId,
                fields = fields'
             } : classFiles,
            ..
        }

    bodyInstrs <- compile' body newEnv
    modify $ \Context{..} -> Context{ 
            depth = currDepth, 
            lastFrame, 
            ..
        }

    return $ 
        maybe (frameInit ++ [Astore 0] ++ varInstrs ++ bodyInstrs)
        (\prefId -> frameInit ++ [
                Dup, 
                Aload 0, 
                Putfield "SL" $ Class prefId,
                Astore 0
            ] ++ varInstrs ++ bodyInstrs ++ [
                Aload 0,
                GetField "SL" $ Class prefId,
                Astore 0
              ]
             ) prefFrame
    where 
        mapFunc (newEnv, fields, fieldId, instrs) Assigment { varName, assignValue } = do
            envDepth   <- currEnvDepth
            fieldInstr <- compile' assignValue newEnv
            let
                varType    = convertType (type' assignValue)
                fieldName  = printf "loc_%d" fieldId
                fieldEntry = (fieldName, varType)

            return ( 
                Map.insert varName VarLoc { 
                        envDepth, 
                        varType,
                        frameLoc = fieldName 
                    } newEnv ,
                fieldEntry : fields,
                fieldId + 1,
                    instrs  
                ++ [Dup] ++ fieldInstr 
                ++ [ Putfield fieldName varType ]
             )

        convertType IntType  = JvmInt
        convertType BoolType = JvmInt
        convertType UnitType = CustomUnit

-- getfields as objects and in the end do cast
-- compile' Ast { node = Var x } env = 
--     case Map.lookup x env of 
--         Just (

currEnvDepth :: CompilerState Int
currEnvDepth = gets depth

compileCompOperations :: Cond -> CompilerState Program
compileCompOperations cond = do
    start <- genLabel
    end   <- genLabel
    return [
      IfIcomp cond start,
      Iconst_0,
      Goto end,
      ILabel start,
      Iconst_1,
      ILabel end
     ]

compilePrints :: TokenValue -> Type -> Program
compilePrints value typ = 
    let 
        printType              = (map toLower $ show value)
        (load, store, jvmType) =  
            case typ of
                IntType  -> (Iload, Istore, "I")
                BoolType -> (Iload, Istore, "Z")
                _        -> (Aload, Astore, "Ljava/lang/Object;")
    in  [
            store 1, -- TODO: if this cause any problem, just save and restore aftwards
            GetStatic "java/lang/System/out" (Class "java/io/PrintStream"),
            load 1,
            Invoke $ printf "java/io/PrintStream/%s(%s)V" printType jvmType,
            pushUnit
        ]
                      
-- loadInstr :: Type -> Int -> Instr
-- loadInstr IntType   = Iload
-- loadInstr BoolType  = Iload
-- loadInstr _ = Aload
--
-- storeInstr :: Type -> Int -> Instr
-- storeInstr IntType   = Istore
-- storeInstr BoolType  = Istore
-- storeInstr _ = Astore

genFrameId :: CompilerState String
genFrameId = do
    id <- gets frameIdCount
    modify $ \Context {..} -> Context{ frameIdCount = id + 1, .. }
    return $ printf "Frame_%d "id

genLabel :: CompilerState Label
genLabel = do
    v <- gets labelCount
    modify $ \ Context {..} -> Context{ labelCount = v + 1, ..}
    return $ "L" ++ show v

{-
 - What is a program now?
 -
 - TODO: for the future add methods and stuffs like that
 -
 - data JvmType   = Basic Type | Class String
 - data ClassFile = ClassFile {
 -      name   :: String,
 -      fields :: [String, Type],
 - }
 - data JvmProgram = ([Instr], [ClassFiles])
 -
 - -}
