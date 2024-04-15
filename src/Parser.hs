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

import Data.List (intercalate)

import qualified Data.List.NonEmpty as List

type TypeToken   = Token
type TypeContext = (Type, TypeToken)

data Assigment = Assigment {
    varName      :: String,
    expectedType :: Maybe TypeContext,
    assignValue  :: Ast
} deriving (Show, Eq)

data Ast = Ast { token :: Token, node :: AstNode } deriving (Eq, Show)

type Condition = Ast
type Body      = Ast
data AstNode = 
            Binary   Ast TokenValue Ast
          | Unary    TokenValue Ast 
          | LetBlock [Assigment] Ast
          | RefAssigment Ast Ast
          | If { condition :: Ast, body :: Ast, elseBody :: Maybe Ast }
          | Var      String
          | Number   Int
          | Bool     Bool
          | Sequence Ast Ast
          | Unit
         deriving (Eq, Show)

-- TODO:: Change all case to maybes c:
type ParserState = ExceptT Error (State [Token])

{-
Context free grammar:

<program>    ::=  <decl> EOF
<decl>       ::= "let" ( Id (":"<type>)? "=" <expr> )+ "in" <decl> | <sequence> 
<sequence>   ::= <assigment> (";" <sequence>)*
<assigment>  ::= <expr> (":=" <assigment>)*
<expr>       ::= <logicalOr>  ( "&&" <logicalOr> )*
<logicalOr>  ::= <comparison> ( "||" <comparison>)*
<comparison> ::= <term>  (( ">" | "<" | "==" | "!=" | ">=" | "<=" ) <term> )*
<term>       ::= <factor> (( "+" | "-" ) <term>  )*
<factor>     ::= <primary> (( "*" | "/" ) <factor> )*
<unary>      ::= ("-"|"~"|"!"|"new") <unary> | <primary>
<primary>    ::= "true" | "false" | Num | "(" ")" | "(" <expr> ")" | ID | <ifExpr> | <printExpr>

<printExpr>  ::= ("print" | "println") <expr>
<ifExpr>     ::= "if" <expr> "then" <decl> ("else" <decl>)? "end"

<type>       ::=  "int" | "bool" | "unit" | "ref" <type>
-}

parse :: [Token] -> Result Ast
parse = evalState (runExceptT parse') 

parse' :: ParserState Ast
parse' = do
    ast <- decl
    consume [EOF] "Expected end of file."
    return ast
   
-- TODO: think about using MaybeT
parseType :: ParserState TypeContext
parseType = do
    case' [REF] (\token -> do 
            subType <- parseType
            return ( RefType $ fst subType, token)
        ) $ do
            typeToken <- consume [INT, BOOL, UNIT] "Invalid type! Expected either 'int', 'bool', or 'unit'."
            return (convert $ value typeToken, typeToken)
    where
        convert INT  = IntType
        convert BOOL = BoolType  
        convert UNIT = UnitType
        
letAssigments :: ParserState [Assigment]
letAssigments = do
    token <- consume [Id ""] "Expected and indentifier after 'let' keyword."
    expectedType <- match [COLON] (return Nothing) $ const (Just <$> parseType)
    
    consume [EQ'] "Expected '=' after variable name."
    assignValue  <- expr

    let 
        Token { value = Id varName } = token
        assign = Assigment { varName, expectedType, assignValue }

    case'  [IN]    (const $ return [assign]) $
     -- TODO: think if there is a need for a check function
     case' [Id ""] (\t -> modify (t:) >> (assign:) <$> letAssigments) $  -- TODO: fix this c:
        makeError "Expected 'in' after variable declaration."

-- decl = match [LET] >>= maybe expr (\_ -> return (Bool True)) -- TODO: think about this 
decl :: ParserState Ast
decl = do
    match [LET] Parser.sequence $ \t -> do
            assigns <- letAssigments
            Ast t . LetBlock assigns <$> decl

sequence :: ParserState Ast 
sequence = do 
    left <- assigment 
    match [SEMI_COLON] (return left) $
        \t -> Ast t . Sequence left <$> Parser.sequence


assigment :: ParserState Ast
assigment = do
    left <- expr 
    match [ASSIGN] (return left) $
        \t -> Ast t . RefAssigment left <$> assigment 
    

expr :: ParserState Ast
expr = do
    left  <- logicalOr -- TODO: think about using flip c:
    match [AND] (return left) $ 
        \t -> Ast t . Binary left (value t) <$> expr

logicalOr :: ParserState Ast
logicalOr = do
    left  <- comparison
    match [OR] (return left) $ 
        \t -> Ast t . Binary left (value t) <$> logicalOr

comparison :: ParserState Ast
comparison = do
    left  <- term 
    match [ GT', LT', GT_EQ, LT_EQ, EQ_EQ, N_EQ ] (return left) $ 
            \t -> Ast t . Binary left (value t) <$> term

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
unary = do -- TODO: think about this
    match [MINUS, NOT, NEW, BANG] primary $
       \t -> Ast t . Unary (value t) <$> unary

primary :: ParserState Ast
primary = do
    token <- gets customHead
    modify customTail
    case value token of 
        LEFT_PAREN -> 
            case' [RIGHT_PAREN] (return . (`Ast` Unit)) $ do
                res <- expr
                consume [RIGHT_PAREN] "Missing enclosing ')'."
                return res

        TRUE      -> return . Ast token $ Bool True
        FALSE     -> return . Ast token $ Bool False
        (Num n)   -> return . Ast token $ Number n
        (Id name) -> return . Ast token $ Var name
        IF        -> do
            condition <- expr
            consume [THEN] "Expected 'then' after if condition."
            body <- decl
            elseBody <- match [ELSE] (return Nothing) (const $ Just <$> decl)
            consume [END] "Expected 'end' at the end of the if then else declaration."

            return . Ast token $ If { condition, body, elseBody }

        x | x `elem` [PRINT, PRINTLN] -> Ast token . Unary x <$> expr 

        _ -> do 
            modify (token:) -- put it bach to the top c:
            makeError "Expected an expression."
         

-- TODO: think about using MaybeT c:
case' :: [TokenValue] -> (Token -> ParserState a) -> ParserState a -> ParserState a
case' expected = flip $ match expected 

match :: [TokenValue] -> ParserState a -> (Token -> ParserState a) -> ParserState a -- ParserState (Maybe Token)
match expected ifNothing ifJust = do
    tokens <- get
    case tokens of 
        (x@(Token value _ _ ):xs) |  any (same value) expected -> do
            put xs
            ifJust x
        _ -> ifNothing

    where
        same (Num _) (Num _) = True
        same (Id _) (Id _)   = True
        same x y = x == y

consume :: [TokenValue] -> String -> ParserState Token
consume expected msg = do
    match expected (makeError msg) return

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
      (x @ Token { value }:xs) -> do
        guard (value `same` expected)
        put xs
        return x
      _ -> guard False
-}
