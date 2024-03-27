module Scanner (
    Token(..),
    tokenize

) where
import qualified Data.Map as Map
import Data.Char (isDigit, isAlpha, isAlphaNum, isSpace)
import Errors (Result)
import Text.Parsec (token)

data Token = 
    -- arithmetic
      PLUS 
    | MINUS
    | SLASH
    | TIMES
    -- comparison
    | OR
    | AND
    | EQ'
    | EQ_EQ
    | N_EQ
    | GT'
    | GT_EQ 
    | LT'
    | LT_EQ 
    -- single character
    | BANG
    -- keywords
    | TRUE
    | FALSE
    | LET
    | IN
    -- parenthesis
    | LEFT_PAREN
    | RIGHT_PAREN 
    -- values
    | Num Int
    | Id String
    -- special c:
    | EOF
 deriving (Eq, Show, Ord)

reservedKeywors :: Map.Map String Token
reservedKeywors = Map.fromList [ 
     ("true", TRUE),
     ("false", FALSE),
     ("let", LET),
     ("in", IN)
    ]

keyword :: String -> Token
keyword word = 
    case Map.lookup word reservedKeywors of
            Just token -> token
            Nothing    -> Id word

tokenize :: String -> [Token]
tokenize [] = [EOF] 
tokenize ('|':'|':xs) = OR    : tokenize xs
tokenize ('&':'&':xs) = AND   : tokenize xs
tokenize ('=':'=':xs) = EQ_EQ : tokenize xs
tokenize ('!':'=':xs) = N_EQ  : tokenize xs
tokenize ('>':'=':xs) = GT_EQ : tokenize xs
tokenize ('<':'=':xs) = LT_EQ : tokenize xs

tokenize ('>':xs) = GT' : tokenize xs
tokenize ('<':xs) = LT' : tokenize xs
tokenize ('=':xs) = EQ' : tokenize xs

tokenize ('+':xs) = PLUS  : tokenize xs
tokenize ('-':xs) = MINUS : tokenize xs
tokenize ('/':xs) = SLASH : tokenize xs
tokenize ('*':xs) = TIMES : tokenize xs

tokenize ('(':xs) = LEFT_PAREN  : tokenize xs
tokenize (')':xs) = RIGHT_PAREN : tokenize xs

tokenize ('!':xs) = BANG : tokenize xs

tokenize txt@(x:xs) 
    | isSpace x = tokenize $ dropWhile isSpace txt -- skip spaces 
    | isAlpha x = keyword word  : tokenize rem
    | isDigit x = Num (read nr) : tokenize rest
    where 
       (word, rem)  = span isAlphaNum txt
       (nr, rest)   = span isDigit txt

tokenize (x:_) = error $ "Unexpected symbol '" ++ [x] ++ "'"