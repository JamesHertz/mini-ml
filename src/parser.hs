module Parser (
    Ast(..),
    AstNode(..),
    Token(..),
    parse,
    Assigment(..)
) where

import {-# SOURCE #-} TypeChecker (Type(..))
-- import Scanner ( Token(..), TokenValue(..) )
import Control.Monad.State (State, get, put, evalState, gets, modify)
import Control.Monad.Except (ExceptT, runExceptT)
import Errors
import Data.Maybe (isNothing)
import Scanner 
import Control.Monad.Trans.Except (throwE)

import qualified Data.List.NonEmpty as List

type TypeToken   = Token
type TypeContext = (Type, TypeToken)

data Assigment = Assigment {
    varName      :: String,
    expectedType :: Maybe TypeContext,
    assignValue  :: Ast
} deriving (Show, Eq)

data Ast = Ast { token :: Token, node :: AstNode } deriving (Eq, Show)

data AstNode = 
            Binary   Ast TokenValue Ast
          | Unary    TokenValue Ast 
          | LetBlock [Assigment] Ast
          | Var      String
          | Number   Int
          | Bool     Bool
          | Unit
         deriving (Eq, Show)

-- TODO:: Change all case to maybes c:
type ParserState = ExceptT Error (State [Token])

{-
Context free grammar:
<program>    ::=  <decl> EOF
<decl>       ::= "let" ( Id (":"<type>)? "=" <expr> )+ "in" <decl> | <expr>
<expr>       ::= <logicalAnd>
<logicalAnd> ::= <logicalOr>  ( "&&" <logicalOr> )*
<logicalOr>  ::= <comparison> ( "||" <comparison>)*
<comparison> ::= <term>  (( ">" | "<" | "==" | "!=" | ">=" | "<=" ) <term> )*
<term>       ::= <factor> (( "+" | "-" ) <factor>  )*
<factor>     ::= <primary> (( "*" | "/" ) <primary> )*
<unary>      ::= ("-"|"~") <unary> | <primary>
<primary>    ::= "true" | "false" | Num | "(" ")" | "(" <expr> ")" | Id

-- helpers c:
<type>       ::=  INT | BOOL | UNIT
-}

parse :: [Token] -> Result Ast
parse = evalState (runExceptT parse') 

parse' :: ParserState Ast
parse' = do
    ast <- decl
    consume [EOF] "Expected end of file."
    return ast
   
-- TODO: think about using MaybeT
parseType :: ParserState (Maybe TypeContext)
parseType = do
    match [COLON] (return Nothing) $ \_ -> do
        typeToken <- consume [INT, BOOL, UNIT] "Expected either 'int', 'bool', or 'unit' type."
        return $ Just (convert $ value typeToken, typeToken)
    where
        convert INT  = IntType
        convert BOOL = BoolType  
        convert UNIT = UnitType
        
letAssigments :: ParserState [Assigment]
letAssigments = do
    token  <- consume [Id ""] "Expected and indentifier after 'let' keyword."
    expectedType <- parseType
    consume [EQ'] "Expected '=' after variable name."
    assignValue  <- expr

    let 
        (Token (Id varName) _ _) = token
        assign    = Assigment { varName, expectedType, assignValue }

    match [IN] ((assign:) <$> letAssigments) $ \_ -> return [assign]

-- decl = match [LET] >>= maybe expr (\_ -> return (Bool True)) -- TODO: think about this 
decl :: ParserState Ast
decl = do
    match [LET] expr $ \l -> do
            assigns <- letAssigments
            Ast l . LetBlock assigns <$> decl

expr :: ParserState Ast
expr = logicalAnd
--
logicalAnd :: ParserState Ast
logicalAnd = do
    left  <- logicalOr -- TODO: think about using flip c:
    match [AND] (return left) $ 
        \t -> Ast t . Binary left (value t) <$> logicalAnd
--
-- -- maybe (return left) (\t -> Binary left t <$> logicalAnd) token -- TODO: think about this
logicalOr :: ParserState Ast
logicalOr = do
    left  <- comparison
    match [OR] (return left) $ 
        \t -> Ast t . Binary left (value t) <$> logicalOr
--
comparison :: ParserState Ast
comparison = do
    left  <- term 
    match [ GT', LT', GT_EQ, LT_EQ, EQ_EQ, N_EQ ] (return left) $ 
            \t -> Ast t . Binary left (value t) <$> comparison

term :: ParserState Ast
term = do
    left  <- factor 
    match [PLUS, MINUS] (return left) $ 
        \t -> Ast t . Binary left (value t) <$> term

factor :: ParserState Ast
factor = do
    left  <- unary
    match [TIMES, SLASH] (return left) $ 
        \t -> Ast t . Binary left (value t) <$> factor

unary :: ParserState Ast
unary = do
    match [MINUS, NOT] primary $ 
        \t -> Ast t . Unary (value t) <$> unary 

primary :: ParserState Ast
primary = do
    token <- gets customHead
    modify customTail
    let (Token value _ _ ) = token
    case value of 
        LEFT_PAREN -> do
            match [RIGHT_PAREN] (do
                res <- expr
                consume [RIGHT_PAREN] "Missing enclosing ')'."
                return res
             ) $ \_ -> return $ Ast token Unit
                
        TRUE      -> return $ Ast token $ Bool True
        FALSE     -> return $ Ast token $ Bool False
        (Num n)   -> return $ Ast token $ Number n
        (Id name) -> return $ Ast token $ Var name
        _ -> do 
            modify (token:) -- put it bach to the top c:
            makeError "Expected an expression."
        

-- Helper functions
-- TODO: think about using MaybeT c:
match :: [TokenValue] -> ParserState a -> (Token -> ParserState a) -> ParserState a -- ParserState (Maybe Token)
match expected ifNothing ifJust = do
    tokens <- get
    case tokens of 
        (x@(Token value _ _ ):xs) | value `elem` expected -> do
            put xs
            ifJust x
        _ -> ifNothing

consume :: [TokenValue] -> String -> ParserState Token
consume expected msg = do
    match expected (makeError msg) return
    -- maybe (makeError msg) return token

makeError :: String -> ParserState a
makeError msg = do
  fst <- gets customHead
  throwE $ Error SyntaxError msg (Scanner.line fst) (Scanner.position fst)

-- to avoid the warning from the compiler c:
customHead :: [c] -> c
customHead = List.head . List.fromList

customTail :: [a] -> [a]
customTail = List.tail . List.fromList

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
