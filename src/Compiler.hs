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
--      - Starting with Baby Steps c:
--          - [ ] Function definition
--          - [ ] Function call
--          - [ ] Particial Application
import Types 
import Control.Monad.State (State, runState, gets, put, modify, foldM, join, when)
import Errors (Result)
import Data.Char (toLower)
import Text.Printf (printf)
import Data.Bifunctor (bimap)

import qualified Data.Map as Map
import Data.Tuple.Extra (fst3, snd3, first)
import Data.Maybe (fromJust)
import Text.Parsec (letter)
import Text.Megaparsec.Byte (letterChar)
import Data.Functor ((<&>))

type JvmProgram = ([Instr], [JvmClass])
-- TODO: think if you really want a Frame type c:
data JvmType    = 
      JvmInt 
    | JvmBool 
    | CustomRef JvmType
    | Class ClassId -- I don't even know what this is doing here c:

instance Show JvmType where
    show JvmInt       = "I"
    show JvmBool      = "Z"
    show (Class id)   = show id

-- stdlib and builting classes
stdUnit = Class $ Default "stdlib/Unit"
stdRef  = Class $ Default "stdlib/Ref" -- TODO: complete this
jvmIntWrapper  = Class $ Default "java/lang/Integer"
jvmBoolWrapper = Class $ Default "java/lang/Boolean"
jvmObjectClass = Class $ Default "java/lang/Object"
jvmPrintStream = Class $ Default "java/io/PrintStream"

toJvmType :: Type -> JvmType
toJvmType  IntType     = JvmInt
toJvmType  BoolType    = JvmBool
toJvmType  UnitType    = stdUnit
toJvmType  (RefType _) = stdRef
toJvmType  (TypeVar _) = jvmBoolWrapper

toJvmWrapper :: Type -> JvmType
toJvmWrapper typ =
    case typ of
        IntType  -> jvmIntWrapper 
        BoolType -> jvmIntWrapper 
        _        -> toJvmType typ

fromClass :: JvmType -> ClassId
fromClass (Class id) = id
-- fromClass other      = error $ "Something is wrong: " ++ show other

data JvmClass = JvmClass {
      classId     :: ClassId,
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

    -- array functions
    | Aaload
    
    | New ClassSpec
    | CheckCast ClassId

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
data ClassId = Frame Int | Func Int | Default String deriving(Ord, Eq)
instance Show ClassId where
    show (Frame id)     = printf "Frame_%d" id
    show (Func id)      = printf "Func_%d" id
    show (Default name) = name

newtype LocId = LocId Int deriving(Ord, Eq)
instance Show LocId where
    show (LocId id) = printf "loc_%d" id

-- used to store static link for frames
staticLinkLocId :: LocId
staticLinkLocId = LocId 0 

-- first locId used for frame fields
startFieldLocId :: Int
startFieldLocId = 1

-- pseudo instructions
ifFalse :: Label -> Instr
ifFalse = Ifeq

pushUnit :: Instr
pushUnit = GetStatic "stdlib/Unit/SINGLE" stdUnit

castToJvmWrapper :: Type -> Instr
castToJvmWrapper typ = CheckCast . fromClass $ toJvmWrapper typ

invokeConstructor :: JvmType -> Instr
invokeConstructor (Class id) = InvokeSpecial $ printf "%s/<init>()V" (show id)

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
invokeMethod (Class id) spec = Invoke $ printf "%s/%s" (show id) spec

putField :: Show a => ClassId -> a -> JvmType -> Instr
putField klass field = PutField $ printf "%s/%s" (show klass) (show field)

getField :: Show a => ClassId -> a -> JvmType -> Instr
getField klass field = GetField $ printf "%s/%s" (show klass) (show field)

-- Types used for state monad and enviroment
data Context = Context {
    labelCount   :: Int,
    frameIdCount :: Int,
    funcIdCount  :: Int,
    depth        :: Int,
    classes      ::  Map.Map ClassId JvmClass,
    lastFrame    :: Maybe ClassId
 }

data VarInfo  = VarInfo {
        envDepth  :: Int,
        frameId   :: ClassId,
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
            funcIdCount  = 0,
            depth        = 0,
            classes      = Map.empty,
            lastFrame    = Nothing
         }
    in (instrs, Map.elems $ classes state)

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
    let 
      fieldsType  = map (toJvmType . type' . assignValue) assigns
      fieldsId    = LocId <$> [startFieldLocId..]
      frameFields = zip fieldsId fieldsType

    prefFrame <- startFrame currFrame frameFields
    depth     <- gets depth
    result    <- foldM (mapFunc depth currFrame) (env, []) 
                  $ zip assigns frameFields

    let 
        newEnv = fst result
        assignsInstrs   = zipWith (\ (fieldId, fieldType)  valueInstrs -> 
                Aload 0 : valueInstrs ++ [ putField currFrame fieldId fieldType ] 
            ) frameFields (snd result)
        
        (staticLinkSetup, staticLinkRestore) = maybe ([],[]) (\ frame -> (
                [ 
                    Dup, Aload 0,
                    putField currFrame staticLinkLocId (Class frame)
                ],
                [ 
                    Aload 0,
                    getField currFrame staticLinkLocId (Class frame),
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
                        fieldId, fieldType
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
                  ++ [ getField frameId fieldId fieldType ]
    where 
        jumpToFrame :: Int -> ClassId -> CompilerState [Instr]
        jumpToFrame 0 _ = return []
        jumpToFrame distance lastFrame = do
            classes  <- gets classes
            let 
               Just JvmClass { fields } = Map.lookup lastFrame classes
               Just fieldType     = Map.lookup staticLinkLocId fields 
               Class currentFrame =  fieldType
            result <- jumpToFrame (distance - 1) currentFrame
            return $ getField lastFrame staticLinkLocId (Class currentFrame) : result


compile' ast@Ast { node = FuncDecl pars body } env = do 
  funcId <- genFuncId

  let 
    FuncType pars result =  type' ast
    fieldsType  = toJvmType <$> pars
    fieldsId    = LocId <$> [startFieldLocId..]
    fields      = zip fieldsId fieldsType
    unwrapInstr = join $ zip fieldsId pars <&> \(LocId idx, typ) -> [
        Aload 1,
        SIpush idx,
        Aaload,
        castToJvmWrapper typ
      ]

  prevFrame <- startFrame funcId fields
  -- do instructions for um wrap c:
    -- prefFrame <- startFrame currFrame frameFields
    -- depth     <- gets depth
  

  return []


-- TODO: handle this
startFrame :: ClassId -> [(LocId, JvmType)] -> CompilerState (Maybe ClassId)
startFrame classId frameVariables =  do
    prefFrame <- gets lastFrame

    -- Add an static link if needed
    let frameClassFields = maybe frameVariables 
            (\frame -> (staticLinkLocId, Class frame) : frameVariables) prefFrame

    modify $ \ Context{ .. } ->  Context { 
        lastFrame  = Just classId, 
        depth      = depth + 1,
        classes     = Map.insert classId JvmClass {
                classId,
                fields  = Map.fromList frameClassFields,
                applyMethod = Nothing
            } classes,
        ..
    }

    return prefFrame

endFrame :: Maybe ClassId -> CompilerState ()
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
            [ GetStatic "java/lang/System/out" jvmPrintStream ],
            [
                invokeMethod jvmPrintStream $ printf "%s(%s)V" printType jvmType,
                -- Invoke $ printf "%s(%s)V" printType jvmType,
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
    castToJvmWrapper typ
  ] ++ unWrapValue typ


genFrameId :: CompilerState ClassId
genFrameId = do
    id <- gets frameIdCount
    modify $ \Context {..} -> Context{ frameIdCount = id + 1, .. }
    return $ Frame id

genFuncId :: CompilerState ClassId
genFuncId = do
    id <- gets funcIdCount
    modify $ \Context {..} -> Context{ funcIdCount = id + 1, .. }
    return $ Func id

genLabel :: CompilerState Label
genLabel = do
    v <- gets labelCount
    modify $ \ Context {..} -> Context{ labelCount = v + 1, ..}
    return $ "L" ++ show v
