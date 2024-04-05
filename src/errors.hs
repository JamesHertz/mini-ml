module Errors(
    Result,
    -- Error(..)
) where

import Control.Monad.State (State)
-- look at this c:
data ErrType = SyntaxError | TypingError

data Error = Error { errType :: ErrType, message :: String, line :: Int, position :: Int }
-- data Error = SyntaxError String | TypingError String

type Result = Either Error-- TODO: fix this later c:


-- reportError :: String -> State 

{-




-}

{-

What types of errors do I have?

-> Types
-> Not definitions c:
-> SyntaxError

-}

-- data Error = SyntaxError String | TypingError String
-- instance Show Error where
--     show (SyntaxError err) = "SyntaxError: " ++ err
--     show (TypingError err) = "Typing: " ++ err