module Compiler(
    Instr(..),
    compile,
    Program,
    Label,
    Cond(..)
) where

import Parser (Ast(..))
import Control.Monad.State (State, evalState, gets, put)


data Cond  = CEQ | CNE | CLT | CGT | CLE | CGE
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
    | ILabel Label

instance Show Cond where
  show CEQ = "eq"
  show CNE = "ne"
  show CLT = "lt"
  show CGT = "gt "
  show CLE = "le"
  show CGE = "ge"

instance Show Instr where
  show IAdd = "iadd"
  show ISub = "isub"
  show IMul = "imul"
  show IDiv = "idiv"
  show IAnd = "iand"
  show IOr  = "ior"
  show INeg = "ineg"
  show (ILabel label) = label ++ ":"
  show (Goto label)   = "goto " ++ label 
  show (SIpush n) = "sipush " ++ show n
  show (IfIcomp cond label) = "if_icmp" ++ show cond ++ " " ++ label

type Program = [Instr]

type CompilerState = State Int 

-- type Ctx = Int
-- makeLabel :: Ctx -> (Ctx, String)
-- makeLabel ctx =  (ctx+1, "L" ++ show ctx)

compile :: Ast -> Program
compile ast =
    evalState (compile' ast) 0

compile' :: Ast -> CompilerState Program
compile' (Number n)   = return [SIpush n]
compile' (Bool value) = return [SIpush $ if value then 1 else 0] 

-- TODO: fix this mess c:
-- compile' (Add   left right) = do
--   left'  <- compile' left 
--   right' <- compile' right 
--   return $ left' ++ right' ++ [IAdd]

-- compile' (Sub left right) = do
--   left'  <- compile' left 
--   right' <- compile' right 
--   return $ left' ++ right' ++ [ISub]

-- compile' (Div left right) = do
--   left'  <- compile' left 
--   right' <- compile' right 
--   return $ left' ++ right' ++ [ISub]

-- compile' (Mult left right) = do
--   left'  <- compile' left 
--   right' <- compile' right 
--   return $ left' ++ right' ++ [IMul]

-- compile' (And left right) = do
--   left'  <- compile' left 
--   right' <- compile' right 
--   return $ left' ++ right' ++ [IAnd]

-- compile' (Or left right) = do
--   left'  <- compile' left 
--   right' <- compile' right 
--   return $ left' ++ right' ++ [IOr]

-- compile' (Neg value ) = do
--   value' <- compile' value
--   return $ value' ++ [INeg]

-- compile' (Equals left right) = do
--   left'  <- compile' left 
--   right' <- compile' right 
--   start <- genLabel
--   end   <- genLabel
--   return $ left' ++ right' ++ [
--      IfIcomp CEQ start,
--      SIpush 0,
--      Goto end,
--      ILabel start,
--      SIpush 1,
--      ILabel end
--    ]

genLabel :: CompilerState Label
genLabel = do
    v <- gets (+1)
    put v
    return $ "L" ++ show v


-- data Ast = -- arithmetic
--             Add  Ast Ast 
--           | Sub  Ast Ast
--           | Mult Ast Ast
--           | Div  Ast Ast
--           | Minus  Ast
--           -- comparison
--           | Neg  Ast
--           | Or   Ast Ast
--           | And  Ast Ast
--           | Equals        Ast Ast
--           | LessThan      Ast Ast
--           | GreaterThan   Ast Ast
--           | LessThanEq    Ast Ast
--           | GreaterThanEq Ast Ast
--           | NotEquals     Ast Ast
--           | LetBlock      [Assigment] Ast
--           -- unary c:
--           | Var String
--         --   | Binary Ast Token Ast -- LATER: plans c:
--           -- values
--           | Number Int
--           | Bool Bool
--          deriving (Eq, Show)

-- compile (Number n)   = [SIpush n]
-- compile (Bool value) = [SIpush $ if value then 1 else 0] 
-- compile (Add   left right) = compile left ++ compile right ++ [IAdd]
-- compile (Sub   left right) = compile left ++ compile right ++ [ISub]
-- compile (Div   left right) = compile left ++ compile right ++ [IDiv] 
-- compile (Mult  left right) = compile left ++ compile right ++ [IMul]
-- compile x = error $ "Doesn't support '" ++ show x ++ "' yet"