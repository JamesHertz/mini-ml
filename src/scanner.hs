module Scanner (
    Token(..),
    tokenize

) where
import qualified Data.Map as Map
import Data.Char (isDigit, isAlpha, isAlphaNum, isSpace)

data Token = 
      PLUS
    | MINUS
    | SLASH
    | TIMES
    | OR
    | AND
    -- | ID String -- not now c:
    | Num Int
    | TRUE
    | FALSE
    | EOF
 deriving (Eq, Show)

reserved :: Map.Map String Token
reserved = Map.fromList [ 
     ("true", TRUE),
     ("false", FALSE)
    ]

keyword :: String -> Token
keyword word = 
    case Map.lookup word reserved of
            Just token -> token
            Nothing    -> error $ "Unexpected keyword '" ++ word ++ "'"

tokenize :: String -> [Token]
tokenize [] = [EOF] 
tokenize ('+':xs) = PLUS  : tokenize xs
tokenize ('-':xs) = MINUS : tokenize xs
tokenize ('/':xs) = SLASH : tokenize xs
tokenize ('*':xs) = TIMES : tokenize xs
tokenize ('|':'|':xs) = OR  : tokenize xs
tokenize ('&':'&':xs) = AND : tokenize xs
tokenize txt@(x:xs) 
    | isSpace x = tokenize $ dropWhile isSpace txt -- skip spaces 
    | isAlpha x = keyword word  : tokenize rem
    | isDigit x = Num (read nr) : tokenize rest
    where 
       (word, rem)  = span isAlphaNum txt
       (nr, rest)   = span isDigit txt

tokenize (x:_) = error $ "Unexpected symbol '" ++ [x] ++ "'"