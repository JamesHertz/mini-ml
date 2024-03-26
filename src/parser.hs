module Parser (
    Ast(..),
    parse,
    Assigment
)where

import Scanner ( Token(..) )

type Assigment = (String, Ast)
data Ast = -- arithmetic
            Add  Ast Ast 
          | Sub  Ast Ast
          | Mult Ast Ast
          | Div  Ast Ast
          | Neg  Ast
          -- comparison
          | Not  Ast
          | Or   Ast Ast
          | And  Ast Ast
          | Equals        Ast Ast
          | LessThan      Ast Ast
          | GreaterThan   Ast Ast
          | LessThanEq    Ast Ast
          | GreaterThanEq Ast Ast
          | NotEquals     Ast Ast
          | LetBlock      [Assigment] Ast
          -- unary c:
          | Var String
        --   | Binary Ast Token Ast -- LATER: plans c:
          -- values
          | Number Int
          | Bool Bool
         deriving (Eq, Show)

{-
Context free grammar:
<program>    ::=  <decl> EOF
<decl>       ::= "let" ( ID "=" <expr> )+ "in" <decl> | <expr>
<expr>       ::= <logicalAnd>
<logicalAnd> ::= <logicalOr>  ( "&&" <logicalOr> )*
<logicalOr>  ::= <comparison> ( "||" <comparison>)*
<comparison> ::= <term>  (( ">" | "<" | "==" | "!=" | ">=" | "<=" ) <term> )*
<term>       ::= <factor> (( "+" | "-" ) <factor>  )*
<factor>     ::= <primary> (( "*" | "/" ) <primary> )*
<unary>      ::= ("-"|"!") <unary> | <primary>
<primary>    ::= "true" | "false" | Num | "(" <expr> ")"
-}

parse :: [Token] -> Ast
parse tokens = 
    let 
        (ast, rest) = decl tokens
    in case rest of
        [EOF] -> ast
        _ -> error "Expression not property ended." -- TODO: improve this...

-- Helper function that takes a tuple and a function
-- and maps the first element of a tuple using the fuction
mapFst :: (a -> b) -> (a,c) -> (b, c)
mapFst f (x, y) = (f x, y)

letAssigments :: [Token] -> ([Assigment], [Token])
letAssigments ((Id name):EQ':ys) = 
    let
        (value, rest) = expr ys
        assigment = (name, value)
    in case rest of
        (IN:ys) -> ([assigment], ys)
        _  ->  mapFst (assigment:) $ letAssigments rest
letAssigments (y:_) = error $ "Expected a variable assigment. But found: " ++ show y

-- TODO: fix this later
decl :: [Token] ->  (Ast, [Token])
decl (LET:xs) = 
    let 
        (assign, rest) = letAssigments xs
    in mapFst (LetBlock assign) $ decl rest
decl xs = expr xs


expr :: [Token] -> (Ast, [Token])
expr =  logicalAnd

logicalAnd :: [Token] -> (Ast, [Token])
logicalAnd tokens =  
    let 
        (ast, rest) = logicalOr tokens
    in case rest of
        (AND:xs) -> mapFst (And ast) $ logicalAnd xs
        _ -> (ast, rest)

-- logicalAnd tokens =  
--     let 
--         (ast, rest) = logicalOr tokens
--     in case rest of
--         (op@AND:xs) -> mapFst (Binary ast op) $ logicalAnd xs
--         _ -> (ast, rest)


logicalOr :: [Token] -> (Ast, [Token])
logicalOr tokens =  
    let 
        (ast, rest) = comparison tokens
    in case rest of
        (OR:xs) -> mapFst (Or ast) $ logicalOr xs
        _ -> (ast, rest)

-- logicalOr :: [Token] -> (Ast, [Token])
-- logicalOr tokens =  
--     let 
--         (ast, rest) = comparison tokens
--     in case rest of
--         (op@OR:xs) -> mapFst (Binary ast op) $ logicalOr xs
--         _ -> (ast, rest)


comparison :: [Token] -> (Ast, [Token])
comparison tokens = 
    let 
        (ast, rest) = term tokens
    in case rest of
        (LT':xs) -> mapFst (LessThan    ast) $ comparison xs
        (GT':xs) -> mapFst (GreaterThan ast) $ comparison xs
        (GT_EQ:xs) -> mapFst (GreaterThanEq ast) $ comparison xs
        (LT_EQ:xs) -> mapFst (LessThanEq    ast) $ comparison xs
        (EQ_EQ:xs) -> mapFst (Equals ast) $ comparison xs
        (N_EQ:xs)  -> mapFst (NotEquals ast) $ comparison xs
        _ -> (ast, rest)

-- TODO: plans (future c:)
-- comparison :: [Token] -> (Ast, [Token])
-- comparison tokens = 
--     let 
--         (ast, rest) = term tokens
--     in case rest of
--         (op:xs) | op `elem` [
--                 GT', LT', GT_EQ, LT_EQ, EQ_EQ
--             ] -> mapFst (Binary ast op) $ comparison xs
--         _ -> (ast, rest)

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
        (ast, rest) = unary tokens
    in case rest of
        (TIMES:xs) -> mapFst (Mult ast) $ factor xs
        (SLASH:xs) -> mapFst (Div  ast) $ factor xs
        _ -> (ast, rest)

unary :: [Token] -> (Ast, [Token])
unary (MINUS:xs) = mapFst Neg $ unary xs
unary (BANG:xs)  = mapFst Not $ unary xs 
unary xs = primary xs

primary :: [Token] -> (Ast, [Token])
primary (TRUE:xs)       = (Bool True, xs)
primary (FALSE:xs)      = (Bool False, xs)
primary ((Num nr):xs)   = (Number nr, xs)
primary ((Id name):xs)  = (Var name, xs)
primary (LEFT_PAREN:xs) = 
    let
        (ast, rest) = expr xs
    in case rest of
        (RIGHT_PAREN:xs) -> (ast, xs)
        _ -> error "Missing enclosing ')'."
primary _ = error "Expected an expression."
-- primary xx = error $ "Expected an expression." ++ show xx