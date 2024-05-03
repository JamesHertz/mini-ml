module Compiler(
    Instr(..),
    compile,
    Program,
    Label,
    Cond(..)
) where

import Parser (BasicAst, Ast(..), AstNode(..))
import TypeChecker (Type(..), typeCheck)
import Scanner (TokenValue(..))
import Control.Monad.State (State, evalState, gets, put)
import Errors (Result)

-- helper types
type Label = String

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

instance Show Instr where
  show IAdd = "iadd"
  show ISub = "isub"
  show IMul = "imul"
  show IDiv = "idiv"
  show IAnd = "iand"
  show IOr  = "ior"
  show INeg = "ineg"
  show Iconst_0 = "iconst_0"
  show Iconst_1 = "iconst_1"
  show (ILabel label) = label ++ ":"
  show (Goto label)   = "goto " ++ label 
  show (SIpush n) = "sipush " ++ show n
  show (IfIcomp cond label) = "if_icmp" ++ show cond ++ " " ++ label

 -- FIXME: delete this later
  show (Print IntType)   = "invokevirtual java/io/PrintStream/println(I)V"
  show (Print BoolType ) = "invokevirtual java/io/PrintStream/println(Z)V"

data Cond  = CEQ | CNE | CLT | CGT | CLE | CGE

instance Show Cond where
  show CEQ = "eq"
  show CNE = "ne"
  show CLT = "lt"
  show CGT = "gt "
  show CLE = "le"
  show CGE = "ge"

type CompilerState = State Int 

type Program = [Instr]

-- TODO: think about this c:
compile :: BasicAst -> Result Program
compile ast = do
    typeCheck ast
    return $ evalState (compile' ast) 0
    -- let program = evalState (compile' ast) 0
    -- return $ program ++ [Print typ]

compile' :: BasicAst -> CompilerState Program
compile' (Ast { node = Number n })   = return [SIpush n]
compile' (Ast { node = Bool value }) = return [SIpush $ if value then 1 else 0] 

compile' (Ast { node = Binary left op right })  = do
  left'  <- compile' left 
  right' <- compile' right 
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

compile' (Ast { node = Unary op value }) = do
    value' <- compile' value
    let instr = case op of
         NOT  -> [INeg] -- FIXME: use if and iconsts c: (someday)
         MINUS -> [INeg]
    return $ value' ++ instr

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

genLabel :: CompilerState Label
genLabel = do
    v <- gets (+1)
    put v
    return $ "L" ++ show v
