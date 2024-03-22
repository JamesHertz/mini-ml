module Scanner (
    Token(..),
    tokenize

) where
import qualified Data.Map as Map
import Data.Char (isDigit, isAlpha, isAlphaNum, isSpace)

data Token = 
    -- arithmetic
      PLUS 
    | MINUS
    | SLASH
    | TIMES
    -- comparison
    | OR
    | AND
    | EQ_EQ
    | GT'
    | GT_EQ 
    | LT'
    | LT_EQ 
    -- keywords
    | TRUE
    | FALSE
    -- parenthesis
    | LEFT_PAREN
    | RIGHT_PAREN 
    -- values
    | Num Int
    -- special c:
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

tokenize ('|':'|':xs) = OR    : tokenize xs
tokenize ('&':'&':xs) = AND   : tokenize xs
tokenize ('=':'=':xs) = EQ_EQ : tokenize xs
tokenize ('>':'=':xs) = GT_EQ : tokenize xs
tokenize ('<':'=':xs) = LT_EQ : tokenize xs

tokenize ('>':xs) = GT' : tokenize xs
tokenize ('<':xs) = LT' : tokenize xs

tokenize ('+':xs) = PLUS  : tokenize xs
tokenize ('-':xs) = MINUS : tokenize xs
tokenize ('/':xs) = SLASH : tokenize xs
tokenize ('*':xs) = TIMES : tokenize xs

tokenize ('(':xs) = LEFT_PAREN  : tokenize xs
tokenize (')':xs) = RIGHT_PAREN : tokenize xs

tokenize txt@(x:xs) 
    | isSpace x = tokenize $ dropWhile isSpace txt -- skip spaces 
    | isAlpha x = keyword word  : tokenize rem
    | isDigit x = Num (read nr) : tokenize rest
    where 
       (word, rem)  = span isAlphaNum txt
       (nr, rest)   = span isDigit txt

tokenize (x:_) = error $ "Unexpected symbol '" ++ [x] ++ "'"