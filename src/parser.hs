module Parser (
    Ast(..),
    parse
)where

import Scanner

data Ast =  Add  Ast Ast 
          | Sub  Ast Ast
          | Mult Ast Ast
          | Or   Ast Ast
          | And  Ast Ast
          | Div  Ast Ast
          | Number Int
          | Bool Bool
         deriving (Eq, Show)

{-
Context free grammar:
<program>    ::=  <expr> EOF
<expr>       ::= <logicalAnd>
<logicalAnd> ::= <logicalOr>  (AND <logicalOr>)*
<logicalOr>  ::= <comparison> (OR <comparison>)*
<comparison> ::= <term> 
<term>       ::= <factor> ((PLUS | MINUS) <factor>  )*
<factor>     ::= <primary> ((TIMES | DIV) <primary> )*
<primary>    ::= TRUE | FALSE | Num | "(" <expr> ")"
-}

parse :: [Token] -> Ast
parse tokens = 
    let 
        (ast, rest) = expr tokens
    in case rest of
        [EOF] -> ast
        _ -> error "Expression not property ended" -- TODO: improve this...


-- Helper function that takes a tuple and a function
-- and maps the first element of a tuple using the fuction
mapFst :: (a -> b) -> (a,c) -> (b, c)
mapFst f (x, y) = (f x, y)

-- TODO: TO BE COMPLETED
expr :: [Token] -> (Ast, [Token])
expr =  logicalAnd

logicalAnd :: [Token] -> (Ast, [Token])
logicalAnd tokens =  
    let 
        (ast, rest) = logicalOr tokens
    in case rest of
        (AND:xs) -> mapFst (And ast) $ logicalAnd xs
        _ -> (ast, rest)

logicalOr :: [Token] -> (Ast, [Token])
logicalOr tokens =  
    let 
        (ast, rest) = comparison tokens
    in case rest of
        (OR:xs) -> mapFst (Or ast) $ logicalOr xs
        _ -> (ast, rest)

-- TODO: TO BE COMPLETED
comparison :: [Token] -> (Ast, [Token])
comparison = term

term :: [Token] -> (Ast, [Token])
term tokens = 
    let 
        (ast, rest) = factor tokens
    in case rest of
        (PLUS:xs)  -> mapFst (Add ast) $ term xs
        (MINUS:xs) -> mapFst (Sub ast) $ term xs
        _ -> (ast, rest)

factor :: [Token] -> (Ast, [Token])
factor tokens = 
    let 
        (ast, rest) = primary tokens
    in case rest of
        (TIMES:xs) -> mapFst (Mult ast) $ factor xs
        (SLASH:xs) -> mapFst (Div  ast) $ factor xs
        _ -> (ast, rest)

primary :: [Token] -> (Ast, [Token])
primary (TRUE:xs)     = (Bool True, xs)
primary (FALSE:xs)    = (Bool False, xs)
primary ((Num nr):xs) = (Number nr, xs)
primary (LEFT_PAREN:xs) = 
    let
        (ast, rest) = expr xs
    in case rest of
        (RIGHT_PAREN:xs) -> (ast, xs)
        _ -> error "Missing enclosing ')'."
primary _ = error "Expected an expression."