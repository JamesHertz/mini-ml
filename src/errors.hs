module Errors(
    Result,
    -- Error(..)
) where

type Result = Either Error-- TODO: fix this later c:

data Error = SyntaxError String | TypingError String

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