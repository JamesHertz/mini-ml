module Parser (
    Ast(..),
    AstNode(..),
    Token(..),
    parse,
    Assigment(..),
    BasicAst
) where

import Control.Monad.State (State, get, put, evalState, gets, modify)
import Control.Monad.Except (ExceptT, runExceptT)
import Errors
import Data.Maybe (isNothing)
import Control.Monad.Trans.Except (throwE)
import Data.List (intercalate)

import Types hiding(TypedAst, type', token)

import qualified Data.List.NonEmpty as List

type ParserState = ExceptT Error (State [Token])

{-
Context free grammar:

<program>    ::=  <decl> EOF
<decl>       ::= "let" ( Id (":"<type>)? "=" <expr> )+ "in" <decl> "end" | <sequence> 
<sequence>   ::= <assigment> (";" <sequence>)*
<assigment>  ::= <expr> (":=" <assigment>)*
<expr>       ::= <logicalOr>  ( "&&" <logicalOr> )*
<logicalOr>  ::= <comparison> ( "||" <comparison>)*
<comparison> ::= <term>  (( ">" | "<" | "==" | "!=" | ">=" | "<=" ) <term> )*
<term>       ::= <factor> (( "+" | "-" ) <term>  )*
<factor>     ::= <primary> (( "*" | "/" ) <factor> )*
<unary>      ::= ("-"|"~"|"!"|"new") <unary> | <primary>
<primary>    ::= "true" | "false" | Num | "(" ")" | "(" <decl> ")" | ID 
                  | <ifExpr> | <printExpr> | <whileExpr>

<printExpr>  ::= ("print" | "println") <expr>
<ifExpr>     ::= "if" <expr> "then" <decl> ("else" <decl>)? "end"
<whileExpr>  ::= "while" <expr> "do" <decl> "end"

<type>       ::=  "int" | "bool" | "unit" | "ref" <type>
-}

parse :: [Token] -> Result BasicAst
parse = evalState (runExceptT parse') 

parse' :: ParserState BasicAst
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
        
letAssigments :: ParserState [Assigment Token]
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
decl :: ParserState BasicAst
decl = do
    match [LET] Parser.sequence $ \t -> do
            assigns <- letAssigments
            result  <- makeAst t . LetBlock assigns <$> decl
            consume [END] "Expected 'end' at the end of a let block."
            return result

sequence :: ParserState BasicAst 
sequence = do 
    left <- assigment 
    match [SEMI_COLON] (return left) $
        \t -> makeAst t . Sequence left <$> Parser.sequence


assigment :: ParserState BasicAst
assigment = do
    left <- expr 
    match [ASSIGN] (return left) $
        \t -> makeAst t . RefAssignment left <$> assigment 
    

expr :: ParserState BasicAst
expr = do
    left  <- logicalOr -- TODO: think about using flip c:
    match [AND] (return left) $ 
        \t -> makeAst t . Binary left (value t) <$> expr

logicalOr :: ParserState BasicAst
logicalOr = do
    left  <- comparison
    match [OR] (return left) $ 
        \t -> makeAst t . Binary left (value t) <$> logicalOr

comparison :: ParserState BasicAst
comparison = do
    left  <- term 
    match [ GT', LT', GT_EQ, LT_EQ, EQ_EQ, N_EQ ] (return left) $ 
            \t -> makeAst t . Binary left (value t) <$> term

term :: ParserState BasicAst
term = do
    left  <- factor 
    match [PLUS, MINUS] (return left) $ 
        \t -> makeAst t . Binary left (value t) <$> term

factor :: ParserState BasicAst
factor = do
    left  <- unary
    match [TIMES, SLASH] (return left) $ 
        \t -> makeAst t . Binary left (value t) <$> factor

unary :: ParserState BasicAst
unary = do -- TODO: think about this
    match [MINUS, NOT, NEW, BANG] primary $
       \t -> makeAst t . Unary (value t) <$> unary

primary :: ParserState BasicAst
primary = do
    token <- gets customHead
    modify customTail
    case value token of 
        LEFT_PAREN -> 
            case' [RIGHT_PAREN] (return . (`makeAst` Unit)) $ do
                res <- decl 
                consume [RIGHT_PAREN] "Missing enclosing ')'."
                return res

        TRUE      -> return . makeAst token $ Bool True
        FALSE     -> return . makeAst token $ Bool False
        (Num n)   -> return . makeAst token $ Number n
        (Id name) -> return . makeAst token $ Var name
        IF        -> do
            condition <- expr
            consume [THEN] "Expected 'then' after if condition."
            body <- decl
            elseBody <- match [ELSE] (return Nothing) (const $ Just <$> decl)
            consume [END] "Expected 'end' at the end of the if then else declaration."

            return . makeAst token $ If { condition, body, elseBody }

        WHILE     -> do
            condition <- expr
            consume [DO] "Expected 'do' after while condition."
            body <- decl
            consume [END] "Expected 'end' at the end of while."
            return . makeAst token $ While condition body

        x | x `elem` [PRINT, PRINTLN] -> makeAst token . Unary x <$> expr 

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
  throwE $ Error SyntaxError msg (Types.line fst) (Types.position fst)

-- to avoid the warning from the compiler c:
customHead :: [c] -> c
customHead = List.head . List.fromList

customTail :: [a] -> [a]
customTail = List.tail . List.fromList

makeAst :: Token -> AstNode Token -> BasicAst
makeAst ctx node = Ast { ctx, node } 
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
