{-# LANGUAGE RecordWildCards #-}
module Compiler(
    Instr(..),
    compile,
    Label,
    Cond(..), 

    JvmProgram,
    JvmType(..),
    JvmClass(..),
    LocId
) where

-- TODO: 
--      - once you finish it all just come here and do some general refactoring c:
--      - reverse your decistion to use Ref to RefObject, RefInt and RefBool | or RefObject and RefPrimitive c:
import Types 
import Control.Monad.State (State, runState, gets, put, modify, foldM, join, when)
import Errors (Result)
import Data.Char (toLower)
import Text.Printf (printf)
import Data.Bifunctor (bimap)

import qualified Data.Map as Map
import Data.Tuple.Extra (fst3, snd3, first)
import Data.Maybe (fromJust)

type JvmProgram = ([Instr], [JvmClass])
-- TODO: think if you really want a Frame type c:
data JvmType    = 
      JvmInt 
    | JvmBool 
    | CustomRef JvmType
    | Frame FrameId 
    | Class String -- I don't even know what this is doing here c:

instance Show JvmType where
    show JvmInt       = "I"
    show JvmBool      = "Z"
    show (Frame id)   = show id
    show (Class name) = name

-- stdlib built classes
stdUnit = Class "stdlib/Unit"
stdRef  = Class "stdlib/Ref" -- TODO: complete this
jvmIntWrapper  = Class "java/lang/Integer"
jvmBoolWrapper = Class "java/lang/Boolean"
jvmObjectClass = Class "java/lang/Object"

toJvmType :: Type -> JvmType
toJvmType  IntType     = JvmInt
toJvmType  BoolType    = JvmBool
toJvmType  UnitType    = stdUnit
toJvmType  (RefType _) = stdRef

data JvmClass = JvmClass {
      name        :: String,
      fields      :: Map.Map LocId JvmType,
      applyMethod :: Maybe [Instr]
} deriving Show

-- helper types
type Label = String
type FieldSpec  = String
type ClassSpec  = String -- TODO: JvmType
type MethodSpec = String

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
    | Ifeq Label
    
    | New ClassSpec
    | CheckCast ClassSpec

    -- FIXME: redefine this and some other instructions
    | PutField FieldSpec JvmType
    | GetField FieldSpec JvmType

    | GetStatic FieldSpec JvmType

    -- TODO: you might need to change the setting below
    | Invoke        MethodSpec
    | InvokeStatic  MethodSpec
    | InvokeSpecial MethodSpec
    | Dup
    | Pop

 deriving Show

data Cond  = CEQ | CNE | CLT | CGT | CLE | CGE
instance Show Cond where
  show CEQ = "eq"
  show CNE = "ne"
  show CLT = "lt"
  show CGT = "gt "
  show CLE = "le"
  show CGE = "ge"

-- types used to identify frames and fields locations/names
newtype FrameId = FrameId Int deriving(Ord, Eq)
instance Show FrameId where
    show (FrameId id) = printf "Frame_%d" id

newtype LocId = LocId Int deriving(Ord, Eq)
instance Show LocId where
    show (LocId id) = printf "loc_%d" id

-- used to store static link for frames
staticLinkLocId :: LocId
staticLinkLocId = LocId 0 

-- first locId used for frame fields
frameFieldsStartLocId :: Int
frameFieldsStartLocId = 1

-- pseudo instructions
ifFalse :: Label -> Instr
ifFalse = Ifeq

pushUnit :: Instr
pushUnit = GetStatic "stdlib/Unit/SINGLE" stdUnit

invokeConstructor :: JvmType -> Instr
invokeConstructor (Class name) = InvokeSpecial $ printf "%s/<init>()V" name

wrapValue :: Type -> [Instr]
wrapValue IntType  = [InvokeStatic "java/lang/Integer/valueOf(I)Ljava/lang/Integer;"]
wrapValue BoolType = [InvokeStatic "java/lang/Boolean/valueOf(Z)Ljava/lang/Boolean;"]
wrapValue _        = []

unWrapValue :: Type -> [Instr]
unWrapValue IntType  = [invokeMethod jvmIntWrapper "intValue()I"]
unWrapValue BoolType = [invokeMethod jvmBoolWrapper "booleanValue()Z"]
unWrapValue _  = []

-- TODO: think about this
invokeMethod :: JvmType -> MethodSpec -> Instr
invokeMethod (Class name) spec = Invoke $ printf "%s/%s" name spec

putField :: Show a => JvmType -> a -> JvmType -> Instr
putField klass field = PutField $ printf "%s/%s" (show klass) (show field)

getField :: Show a => JvmType -> a -> JvmType -> Instr
getField klass field = GetField $ printf "%s/%s" (show klass) (show field)

getFrameField frame = getField (Frame frame)
putFrameField frame = putField (Frame frame)

-- Types used for state monad and enviroment
data Context = Context {
    labelCount   :: Int,
    frameIdCount :: Int,
    depth        :: Int,
    -- TODO: add this later
    -- classFiles   :: [JvmClass]
    frames       ::  Map.Map FrameId JvmClass,
    lastFrame    :: Maybe FrameId
 }

data VarInfo  = VarInfo {
        envDepth  :: Int,
        frameId   :: FrameId,
        fieldId   :: LocId,
        fieldType :: JvmType
    } deriving Show

type CompilerEnv   = Map.Map String VarInfo
type CompilerState = State Context

-- TODO: think about this c:
compile :: TypedAst -> JvmProgram
compile ast = 
    let 
        (instrs, state)  = runState (compile' ast Map.empty) $ Context {
            labelCount   = 0,
            frameIdCount = 0,
            depth        = 0,
            frames       = Map.empty,
            lastFrame    = Nothing
         }
    in (instrs, Map.elems $ frames state)

compile' :: TypedAst -> CompilerEnv -> CompilerState [Instr]
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

compile' Ast { node = While cond body } env = do
    condInstrs <- compile' cond env
    bodyInstrs <- compile' body env
    loop <- genLabel
    end  <- genLabel
    return $   ILabel loop : condInstrs
            ++ ifFalse end : bodyInstrs
            ++ [ 
                Pop, 
                Goto loop, 
                ILabel end, 
                pushUnit 
            ]

compile' Ast { node = Unary op value } env = do
    value' <- compile' value env
    let (before, after) = case op of
         NOT   -> ([], [INeg]) -- FIXME: use if and iconsts c: (someday)
         MINUS -> ([], [INeg])
         PRINTLN -> compilePrints op (type' value)
         PRINT   -> compilePrints op (type' value)
         NEW     -> compileNew $ type' value
         BANG    -> ([], compileDeref $ type' value)
    return $ before ++ value' ++ after

compile' Ast { node = RefAssignment ref value } env = do
    refInstrs   <- compile' ref env
    valueInstrs <- compile' value env
    return $   refInstrs 
             ++ valueInstrs
             ++ wrapValue (type' value)
             ++ [
                Dup,
                Astore 1,
                PutField (printf "%s/value" $ show stdRef) jvmObjectClass,
                Aload 1
             ]
             ++ unWrapValue (type' value)

compile' Ast { node = LetBlock assigns body } env = do
    currFrame <- genFrameId
    let frameVariables = zipWith (curry $ bimap LocId toJvmType ) [frameFieldsStartLocId..] $ 
                             map (type' . assignValue) assigns

    prefFrame <- startFrame currFrame frameVariables
    depth     <- gets depth
    result    <- foldM (mapFunc depth currFrame) (env, []) 
                  $ zip assigns frameVariables

    let 
        newEnv = fst result
        assignsInstrs   = zipWith (\ (fieldId, fieldType)  valueInstrs -> 
                Aload 0 : valueInstrs ++ [ putFrameField currFrame fieldId fieldType ] 
            ) frameVariables (snd result)
        
        (staticLinkSetup, staticLinkRestore) = maybe ([],[]) (\ frame -> (
                [ 
                    Dup, Aload 0,
                    putFrameField currFrame staticLinkLocId (Frame frame)
                ],
                [ 
                    Aload 0,
                    getFrameField currFrame staticLinkLocId (Frame frame),
                    Astore 0
                ]
            )) prefFrame

    bodyInstrs <- compile' body newEnv 
    endFrame prefFrame
    return  $ [
            New $ show currFrame,
            Dup,
            InvokeSpecial $ printf "%s/<init>()V" (show currFrame)
         ] 
        ++ staticLinkSetup
        ++ [ Astore 0 ]
        ++ join assignsInstrs 
        ++ bodyInstrs
        ++ staticLinkRestore
    where 
        mapFunc envDepth frameId (newEnv, fieldInstrs) (
                Assigment { varName, assignValue }, (fieldId, fieldType)
         ) = do
            instrs  <- compile' assignValue newEnv
            return ( 
                Map.insert varName VarInfo { 
                        frameId, envDepth, 
                        fieldId, fieldType -- TODO: should I remove the type?
                    } newEnv,
                    fieldInstrs ++ [instrs]
             )

compile' Ast { node = Var name } env = do 
    case Map.lookup name env of 
        Nothing -> error $ printf "BUG!! Unable to get infor for var '%s'" name
        Just VarInfo {
            envDepth, fieldId, 
            fieldType, frameId
         }      ->  do

         currDepth <- gets depth
         lastFrame <- gets lastFrame
         result    <- jumpToFrame (currDepth - envDepth)  (fromJust lastFrame)
         return $    Aload 0 : result 
                  ++ [ getFrameField frameId fieldId fieldType ]
    where 
        jumpToFrame :: Int -> FrameId -> CompilerState [Instr]
        jumpToFrame 0 _ = return []
        jumpToFrame distance lastFrame = do
            frames  <- gets frames
            let 
               Just JvmClass { name, fields } = Map.lookup lastFrame frames
               Just fieldType     = Map.lookup staticLinkLocId fields 
               Frame currentFrame =  fieldType
            result <- jumpToFrame (distance - 1) currentFrame
            return $ getFrameField lastFrame staticLinkLocId (Frame currentFrame) : result

startFrame :: FrameId -> [(LocId, JvmType)] -> CompilerState (Maybe FrameId)
startFrame frameId frameVariables =  do
    prefFrame <- gets lastFrame

    -- Add an static link if needed
    let frameClassFields = maybe frameVariables 
            (\frame -> (staticLinkLocId, Frame frame) : frameVariables) prefFrame

    modify $ \ Context{ .. } ->  Context { 
        lastFrame  = Just frameId, 
        depth      = depth + 1,
        frames     = Map.insert frameId JvmClass {
                name   = show frameId,
                fields = Map.fromList frameClassFields,
                applyMethod = Nothing
            } frames,
        ..
    }

    return prefFrame

endFrame :: Maybe FrameId -> CompilerState ()
endFrame lastFrame = 
    modify $ \ Context { .. } -> Context {
        depth = depth - 1,
        lastFrame,
        ..
    }

compileCompOperations :: Cond -> CompilerState [Instr]
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

compilePrints :: TokenValue -> Type -> ([Instr],  [Instr])
compilePrints value typ = 
    let 
        printType              = (map toLower $ show value)
        (load, store, jvmType) =  
            case typ of
                IntType  -> (Iload, Istore, "I")
                BoolType -> (Iload, Istore, "Z")
                _        -> (Aload, Astore, "Ljava/lang/Object;")
    in  (
            [ GetStatic "java/lang/System/out" $ Class "java/io/PrintStream" ],
            [
                Invoke $ printf "java/io/PrintStream/%s(%s)V" printType jvmType,
                pushUnit
            ]
        )
                      
compileNew :: Type -> ([Instr], [Instr])
compileNew valueType' = 
  (
    [
        New $ show stdRef,
        Dup, -- one for init
        Dup, -- and one for put field (the last one is because its the value)
        invokeConstructor stdRef
    ],
        wrapValue valueType' 
    ++ [ PutField (printf "%s/value" $ show stdRef) jvmObjectClass ]
  )

compileDeref :: Type -> [Instr]
compileDeref (RefType typ) = [
    GetField (printf "%s/value" $ show stdRef) jvmObjectClass,
    CheckCast (show castType)
  ] ++ unWrapValue typ
  where 
    castType = case typ of
        IntType  -> jvmIntWrapper 
        BoolType -> jvmIntWrapper 
        _        -> toJvmType typ

genFrameId :: CompilerState FrameId
genFrameId = do
    id <- gets frameIdCount
    modify $ \Context {..} -> Context{ frameIdCount = id + 1, .. }
    return $ FrameId id

genLabel :: CompilerState Label
genLabel = do
    v <- gets labelCount
    modify $ \ Context {..} -> Context{ labelCount = v + 1, ..}
    return $ "L" ++ show v
