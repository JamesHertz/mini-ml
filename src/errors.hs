module Errors where

import Control.Monad.State (State)
import Data.Traversable (for)
-- look at this c:
data ErrType = SyntaxError | TypingError deriving(Show)

data Error = Error { 
    errType  :: ErrType, 
    message  :: String, 
    line     :: Int, 
    position :: Int 
 } deriving(Show)

type Result = Either Error-- TODO: fix this later c:

formatErr :: String -> Error -> String
formatErr src Error { errType, message, line, position } =
    let 
        (init, tail) = splitAt position src
        lineInit     = reverse . takeWhile (/= '\n') . reverse $ init
        start        = " " ++ show line ++ "| " ++ lineInit
        end          = takeWhile (/= '\n') tail
        srcLine      = start ++ end 
        indication   = replicate (length start) ' ' ++ "^ here"
        errLine      = formatErrType errType ++ message
    in errLine ++ "\n" ++ srcLine ++ "\n" ++ indication
    where
        formatErrType SyntaxError = "Syntax error: " 
        formatErrType TypingError = "Typing error: " 
