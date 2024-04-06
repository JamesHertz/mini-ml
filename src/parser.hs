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

{-
TODO: think about this

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