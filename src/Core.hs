module Core where

import Errors
import Compiler (compile, Program)
import Interpreter (eval, Value (..))
import Scanner (tokenize)
import Parser (parse)
import TypeChecker (typeCheck)

compileProgram :: String -> Result Program
compileProgram src = do
    tokens <- tokenize src
    ast    <- parse tokens
    compile ast

interpretProgram :: String -> Result Value
interpretProgram src =  do
    tokens <- tokenize src
    ast    <- parse tokens
    typeCheck ast
    return $ eval ast
