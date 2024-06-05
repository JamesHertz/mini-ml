module Core where

import Errors
import Compiler (compile, JvmProgram(..))
import Interpreter (eval, Value (..))
import Scanner (tokenize)
import Parser (parse)
-- import TypeChecker (typeCheck)
import Unification (typeCheck)

compileProgram :: String -> Result JvmProgram
compileProgram src = do
    tokens   <- tokenize src
    ast      <- parse tokens 
    typedAst <- typeCheck ast
    return $ compile typedAst

interpretProgram :: String -> Result (IO Value)
interpretProgram src =  do
    tokens <- tokenize src
    ast    <- parse tokens
    typeCheck ast
    return $ eval ast
