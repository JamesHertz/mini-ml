module Errors(
    Result,
    -- Error(..)
) where

type Result = Either String -- TODO: fix this later c:
-- data Error = SyntaxError String | TypingError String
-- instance Show Error where
--     show (SyntaxError err) = "SyntaxError: " ++ err
--     show (TypingError err) = "Typing: " ++ err