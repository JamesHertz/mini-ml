module Errors(
    Result,
    -- Error(..)
) where

-- look at this c:
data Error = SyntaxError String | TypingError String

type Result = Either String-- TODO: fix this later c:


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