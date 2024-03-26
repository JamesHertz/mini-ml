module Compiler(
    Instr(..),
    compile,
    Program
) where

import Parser (Ast(..))
data Instr = 
      IAdd
    | ISub
    | IDiv
    | IMul
    | SIpush Int
    | Label String

type Program = [Instr]

instance Show Instr where
  show IAdd = "iadd"
  show ISub = "isub"
  show IMul = "imul"
  show IDiv = "idiv"
  show (SIpush n) = "sipush " ++ show n

-- type Ctx = Int
-- makeLabel :: Ctx -> (Ctx, String)
-- makeLabel ctx =  (ctx+1, "L" ++ show ctx)

compile :: Ast -> Program
compile (Number n)   = [SIpush n]
compile (Bool value) = [SIpush $ if value then 1 else 0] 
compile (Add   left right) = compile left ++ compile right ++ [IAdd]
compile (Sub   left right) = compile left ++ compile right ++ [ISub]
compile (Div   left right) = compile left ++ compile right ++ [IDiv] 
compile (Mult  left right) = compile left ++ compile right ++ [IMul]
compile x = error $ "Doesn't support '" ++ show x ++ "' yet"