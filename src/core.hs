module Core where

import Errors
import Compiler
import Interpreter (Value)


compileProgram :: String -> Result Program
compileProgram _ = Right []

interpretProgram :: String -> Result Value
interpretProgram _ = Left "To be implemented"