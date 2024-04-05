module Core where

import Errors
import Compiler
import Interpreter (Value (IntValue))


compileProgram :: String -> Result Program
compileProgram _ = Right []

interpretProgram :: String -> Result Value
interpretProgram _ = Right $ IntValue 10