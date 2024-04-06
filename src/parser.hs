module Parser (
    Ast(..),
    Token(..),
    parse,
    Assigment
)where

-- import Scanner ( Token(..), TokenValue(..) )
import Control.Monad.State (State, get, put, evalState, gets, modify)
import Control.Monad.Except (ExceptT, runExceptT)
import Errors
import Data.Maybe (isNothing)
import Tests
import Control.Monad.Trans.Except (throwE)

import qualified Data.List.NonEmpty as List

-- 
type Assigment = (String, Token, Ast)
data Ast = 
            Binary Ast Token Ast
          | Unary  Token Ast 
          | LetBlock [Assigment] Ast
          | Var String
          | Number Int
          | Bool Bool
         deriving (Eq, Show)

-- type ParserState = State [Token]

-- TODO:: Change all case to maybes c:
type ParserState = ExceptT Error (State [Token])

{-
Context free grammar:
<program>    ::=  <decl> EOF
<decl>       ::= "let" ( Id (":"Id)? "=" <expr> )+ "in" <decl> | <expr>
<expr>       ::= <logicalAnd>
<logicalAnd> ::= <logicalOr>  ( "&&" <logicalOr> )*
<logicalOr>  ::= <comparison> ( "||" <comparison>)*
<comparison> ::= <term>  (( ">" | "<" | "==" | "!=" | ">=" | "<=" ) <term> )*
<term>       ::= <factor> (( "+" | "-" ) <factor>  )*
<factor>     ::= <primary> (( "*" | "/" ) <primary> )*
<unary>      ::= ("-"|"!") <unary> | <primary>
<primary>    ::= "true" | "false" | Num | "(" <expr> ")" | Id
-}


parse :: [Token] -> Result Ast
parse = evalState (runExceptT parse') 

parse' :: ParserState Ast
parse' = do
    ast <- decl
    consume EOF "Expected end of file."
    return ast
    
--
-- decl :: ParserState Ast
-- decl = return $ Number 10


-- TODO: think about using MaybeT c:
match :: [TokenValue] -> ParserState (Maybe Token)
match expected = do
    tokens <- get
    case tokens of 
        (x@(Token value _ _ ):xs) | value `elem` expected -> do
            put xs
            return $ Just x
        _ -> return Nothing

consume :: TokenValue -> String -> ParserState Token
consume expected msg = do
    token <- match [expected]
    maybe (makeError msg) return token

makeError :: String -> ParserState a
makeError msg = do
  fst <- gets customHead
  throwE $ Error SyntaxError msg (Tests.line fst) (Tests.position fst)

--
-- advance :: ParserState ()
-- advance = do
--     tokens <- get
--     case tokens of
--         (x:xs) -> put xs
--         _      -> return ()
--
{-

type MaybeParserState = MaybeT (State [Token])
type ParserState      = ExceptT Error (State [Token])

match :: TokenValue -> MaybeParseState Token
match expected = do
    tokens <- get
    case tokens of 
      (x@(Token value _ _):xs) -> do
        guard (value == expected)
        put xs
        return x
      _ -> guard False
-}


    



-- consume :: Token -> Message -> ParserState Token
-- consume expected msg = do
--     t <- match [expected]
--     case t of
--         Just t'  -> return t'
--         Nothing  -> error msg
-------------------------- 
-- previous thing
-------------------------- 

-- the real thing
-- parse :: [Token] -> Ast
-- parse tokens = 
--     let 
--         (ast, rest) = runState decl tokens
--     in case rest of
--         [EOF] -> ast
--         xs -> error $ "Expression not property ended: " ++ show xs -- TODO: improve this...
--



letAssigments :: ParserState [Assigment]
letAssigments = do
    token  <- consume (Id "") "Expected and indentifier after 'let' keyword."
    eqSign <- consume EQ' "Expected '=' after variable name."
    value  <- expr

    let 
        (Token (Id name) _ _) = token
        assign    = (name, eqSign, value)
    inToken <- match [IN]
    if isNothing inToken then
        (assign:) <$> letAssigments
    else 
        return [assign]

-- decl = match [LET] >>= maybe expr (\_ -> return (Bool True)) -- TODO: think about this 
decl :: ParserState Ast
decl = do
    token <- match [LET]
    case token of
        Just t -> do
            assigns <- letAssigments
            LetBlock assigns <$> decl
        _ -> expr

expr :: ParserState Ast
expr = logicalAnd

logicalAnd :: ParserState Ast
logicalAnd = do
    left  <- logicalOr
    token <- match [AND]
    case token of
        Just t -> Binary left t <$> logicalAnd
        _ -> return left

-- maybe (return left) (\t -> Binary left t <$> logicalAnd) token -- TODO: think about this
logicalOr :: ParserState Ast
logicalOr = do
    left  <- comparison
    token <- match [OR]
    case token of
        Just t -> Binary left t <$> logicalOr
        _ -> return left

comparison :: ParserState Ast
comparison = do
    left  <- term 
    token <- match [ GT', LT', GT_EQ, LT_EQ, EQ_EQ, N_EQ ]
    case token of
        Just t -> Binary left t <$> comparison
        _ -> return left

term :: ParserState Ast
term = do
    left  <- factor 
    token <- match [PLUS, MINUS]
    case token of
        Just t -> Binary left t <$> term
        _ -> return left

factor :: ParserState Ast
factor = do
    left  <- unary
    token <- match [TIMES, SLASH]
    case token of
        Just t -> Binary left t <$> factor
        _ -> return left

unary :: ParserState Ast
unary = do
    token <- match [MINUS, BANG]
    case token of
        Just t -> Unary t <$> unary 
        _  -> primary

primary :: ParserState Ast
primary = do
    token <- gets customHead
    modify customTail
    let (Token value _ _ ) = token
    case value of 
        LEFT_PAREN -> do
            res <- expr
            consume RIGHT_PAREN "Missing enclosing ')'."
            return res
        TRUE      -> return $ Bool True
        FALSE     -> return $ Bool False
        (Num n)   -> return $ Number n
        (Id name) -> return $ Var name
        _ -> do 
            modify (token:) -- put it bach to the top c:
            makeError "Expected an expression."


-- to avoid the warning from the compiler c:
customHead :: [c] -> c
customHead = List.head . List.fromList

customTail :: [a] -> [a]
customTail = List.tail . List.fromList

-- -- helper function c:
-- match :: [Token] -> ParserState (Maybe Token)
-- match expected = do
--     tokens <- get
--     case tokens of
--         (x:xs) | x `elem` expected -> put xs >> return (Just x)
--         _ -> return Nothing
--
-- type Message = String
-- consume :: Token -> Message -> ParserState Token
-- consume expected msg = do
--     t <- match [expected]
--     case t of
--         Just t'  -> return t'
--         Nothing  -> error msg
--    
-- -- TODO: think about this c:

-- check :: TokenValue -> (TokenValue -> a) -> ParserState a


-- takeToken :: ParserState Token
-- takeToken = do
--     tokens <- get
--     let (head:tail) = tokens
--     put tail
--     return head
--
--------------------------------------------
--  *------------------------------------*
--  | Hi there                           |
--  *------------------------------------*
--------------------------------------------

-- -- OLD parser: FIXME: delete this someday
-- mapFst :: (a -> c) -> (a, b) -> (c, b)
-- mapFst f (x,y) = (f x, y)
--
-- letAssigments :: [Token] -> ([Assigment], [Token])
-- letAssigments ((Id name):EQ':ys) = 
--     let
--         (value, rest) = expr ys
--         assigment = (name, value)
--     in case rest of
--         (IN:ys) -> ([assigment], ys)
--         _  ->  mapFst (assigment:) $ letAssigments rest
-- letAssigments (y:_) = error $ "Expected a variable assigment. But found: " ++ show y

-- TODO: fix this later
-- decl :: [Token] ->  (Ast, [Token])
-- decl (LET:xs) = 
--     let 
--         (assign, rest) = letAssigments xs
--     in mapFst (LetBlock assign) $ decl rest
-- decl xs = expr xs

-- expr :: [Token] -> (Ast, [Token])
-- expr =  logicalAnd

-- logicalAnd :: [Token] -> (Ast, [Token])
-- logicalAnd tokens =  
--     let 
--         (ast, rest) = logicalOr tokens
--     in case rest of
--         (op@AND:xs) -> mapFst (Binary ast op) $ logicalAnd xs
--         _ -> (ast, rest)

-- logicalOr :: [Token] -> (Ast, [Token])
-- logicalOr tokens =  
--     let 
--         (ast, rest) = comparison tokens
--     in case rest of
--         (op@OR:xs) -> mapFst (Binary ast op) $ logicalOr xs
--         _ -> (ast, rest)

-- -- TODO: plans (future c:)
-- comparison :: [Token] -> (Ast, [Token])
-- comparison tokens = 
--     let 
--         (ast, rest) = term tokens
--     in case rest of
--         (op:xs) | op `elem` [
--                 GT', LT', GT_EQ, LT_EQ, EQ_EQ, N_EQ
--             ] -> mapFst (Binary ast op) $ comparison xs
--         _ -> (ast, rest)

-- term :: [Token] -> (Ast, [Token])
-- term tokens = 
--     let 
--         (ast, rest) = factor tokens
--     in case rest of
--         (op:xs) | op `elem` [
--             PLUS, MINUS
--             ] -> mapFst (Binary ast op) $ term xs
--         -- (PLUS:xs)  -> mapFst (Add ast) $ term xs
--         -- (MINUS:xs) -> mapFst (Sub ast) $ term xs
--         _ -> (ast, rest)

-- factor :: [Token] -> (Ast, [Token])
-- factor tokens = 
--     let 
--         (ast, rest) = unary tokens
--     in case rest of
--         (op:xs) | op `elem` [
--             TIMES, SLASH
--             ] -> mapFst (Binary ast op) $ term xs
--         -- (TIMES:xs) -> mapFst (Mult ast) $ factor xs
--         -- (SLASH:xs) -> mapFst (Div  ast) $ factor xs
--         _ -> (ast, rest)

-- unary :: [Token] -> (Ast, [Token])
-- -- unary (MINUS:xs) = mapFst Minus $ unary xs
-- -- unary (BANG:xs)  = mapFst Neg $ unary xs 
-- unary (op@MINUS:xs) = mapFst (Unary op) $ unary xs
-- unary (op@BANG:xs)  = mapFst (Unary op) $ unary xs 
-- unary xs = primary xs

-- primary :: [Token] -> (Ast, [Token])
-- primary (TRUE:xs)       = (Bool True, xs)
-- primary (FALSE:xs)      = (Bool False, xs)
-- primary ((Num nr):xs)   = (Number nr, xs)
-- primary ((Id name):xs)  = (Var name, xs)
-- primary (LEFT_PAREN:xs) = 
--     let
--         (ast, rest) = expr xs
--     in case rest of
--         (RIGHT_PAREN:xs) -> (ast, xs)
--         _ -> error "Missing enclosing ')'."
-- primary _ = error "Expected an expression."
-- primary xx = error $ "Expected an expression." ++ show xx
