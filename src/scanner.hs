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
 deriving (Show, Ord)



instance Eq Token where
    -- I needed the two definition below, because of the consume
    -- function defined at 'parser.hs' so I had to define Eq by hand c:
    Num _ == Num _ = True
    Id  _ == Id _  = True

    PLUS  == PLUS  = True 
    MINUS == MINUS = True
    SLASH == SLASH = True
    TIMES == TIMES = True

    OR  == OR  = True
    AND == AND = True
    EQ' == EQ' = True
    GT' == GT' = True
    LT' == LT' = True

    EQ_EQ == EQ_EQ = True
    N_EQ  == N_EQ  = True
    GT_EQ == GT_EQ = True 
    LT_EQ == LT_EQ = True 
    BANG  == BANG  = True

    TRUE  == TRUE  = True
    FALSE == FALSE = True

    LEFT_PAREN  == LEFT_PAREN = True
    RIGHT_PAREN == RIGHT_PAREN = True 

    LET == LET = True
    IN  == IN = True
    EOF == EOF = True

    _ == _ = False


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