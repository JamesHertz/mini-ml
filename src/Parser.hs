module Parser (
    Ast(..),
    AstNode(..),
    Token(..),
    parse,
    Assigment(..),
    BasicAst, 
    Parameter 
) where

import Control.Monad.State (State, get, put, evalState, gets, modify)
import Control.Monad.Except (ExceptT, runExceptT)
import Errors
import Data.Maybe (isNothing)
import Control.Monad.Trans.Except (throwE)
import Data.List (intercalate)

import Types hiding(TypedAst, type')

import qualified Data.List.NonEmpty as List
import Control.Lens (Field10(_10))

type ParserState = ExceptT Error (State [Token])

{-
Context free grammar:

<program>    ::=  <decl> EOF
<decl>       ::=  <sequence> | <letBlock> | <whileExpr> | <ifExpr>
<letBlock>   ::= "let" ( Id (":"<type>)? "=" <decl> )+ "in" <decl> "end" 
<funDecl>    ::= "fun" ( ( "(" Id ":" <type> ")" )* | "()" ) (":" <type>)"->" <decl> "end"
<sequence>   ::= <assigment> (";" <sequence>)*
<assigment>  ::= <expr> (":=" <assigment>)?
<expr>       ::= <logicalOr>  ( "&&" <logicalOr> )*
<logicalOr>  ::= <comparison> ( "||" <comparison>)*
<comparison> ::= <term>  (( ">" | "<" | "==" | "!=" | ">=" | "<=" ) <term> )*
<term>       ::= <factor> (( "+" | "-" ) <term>  )*
<factor>     ::= <primary> (( "*" | "/" ) <factor> )*
<unary>      ::= ("-"|"~"|"!"|"new"|"print" | "println") <unary> | <call>
<call>       ::= <primary> (<primary>)*
<primary>    ::= "true" | "false" | Num | "()" | "(" <decl> ")" | ID 
                  
<ifExpr>     ::= "if" <expr> "then" <decl> ("else" <decl>)? "end"
<whileExpr>  ::= "while" <expr> "do" <decl> "end"

<type>       ::=  "int" | "bool" | "unit" | "ref" <type> | <type> "->" <type>


<expr> (<primary>)+
-}

parse :: [Token] -> Result BasicAst
parse = evalState (runExceptT parse') 

parse' :: ParserState BasicAst
parse' = do
    ast <- decl
    consume [EOF] "Expected end of file."
    return ast
   
-- decl = match [LET] >>= maybe expr (\_ -> return (Bool True)) -- TODO: think about this 
decl :: ParserState BasicAst
decl = case' [LET]   letBlock  $
       case' [FUN]   funDecl   $
       case' [WHILE] whileExpr $ 
       case' [IF]    ifExpr Parser.sequence

funDecl :: Token -> ParserState BasicAst
funDecl funKeyword = do 
    -- TODO: add support for free variables c:
    params <- case' [UNIT_VALUE] (\t -> return [("", t, Just UnitType)]) $
              case' [LEFT_PAREN, Id ""] funParams $
              makeError "Expected either '()' or variable declaration."

    consume [ARROW] "Expected '->' before function body!"
    result  <- makeAst funKeyword . FuncDecl params <$> decl
    consume [END] "Expected 'end' at the end of function declaration."
    return result

funParams :: Token -> ParserState [Parameter]
funParams prev = do
    -- TODO: think about being able to type the return type like this: `fun (x : int) : int -> x * 10 end`
    entry <- case value prev of 
               LEFT_PAREN -> 
                do
                    par <- consume [Id ""] "Expected function parameter name after '(' in function declaration."
                    consume [COLON] "Expected ':' after parameter name."
                    typeInfo <- parseType
                    consume [RIGHT_PAREN] "Expected enclosing ')' after function parameter name and type."
                    let (Id varName) = value par
                    return (varName, prev, Just $ fst typeInfo)
               Id name    -> return (name, prev, Nothing)

    rest <- match [LEFT_PAREN, Id ""] (return []) funParams
    return $ entry : rest


letBlock :: Token ->  ParserState BasicAst
letBlock letKeyword = do
    assigns <- letAssigments
    result  <- makeAst letKeyword . LetBlock assigns <$> decl
    consume [END] "Expected 'end' at the end of a let block."
    return result

letAssigments :: ParserState [Assigment Token]
letAssigments = do
    token <- consume [Id ""] "Expected and indentifier after 'let' keyword."
    expectedType <- match [COLON] (return Nothing) $ const (Just <$> parseType)
    
    consume [EQ'] "Expected '=' after variable name."

    assignValue  <- decl

    let 
        Token { value = Id varName } = token
        assign = Assigment { varName, expectedType, assignValue }

    case'  [IN]    (const $ return [assign]) $
     -- TODO: think if there is a need for a ) checkfunction
     case' [Id ""] (\t -> modify (t:) >> (assign:) <$> letAssigments) $  -- TODO: fix this c:
        makeError "Expected 'in' after variable declaration."

-- TODO: think about using MaybeT
parseType :: ParserState TypeContext
parseType = do
    result <- parseBasicType
    match [ARROW] (return result) . const $ do
            outType <- match [REF, INT, BOOL, UNIT] 
                        (makeError "Expected either 'ref', 'int', 'bool' or 'unit' after type arrow!") $ 
                        \t -> modify (t:) >> parseType
            let 
                (inType, typeToken) = result
                resultType = case fst outType of
                                FuncType in' out -> FuncType (inType : in') out
                                out              -> FuncType [inType] out
            return (resultType, typeToken)
    where
        parseBasicType = case' [REF] (\token -> do 
                            subType <- parseType
                            return ( RefType $ fst subType, token)
                     ) $ do typeToken <- consume [INT, BOOL, UNIT] 
                                "Invalid type! Expected either 'int', 'bool', or 'unit'."
                            return (convert $ value typeToken, typeToken)
        convert INT  = IntType
        convert BOOL = BoolType  
        convert UNIT = UnitType

whileExpr :: Token -> ParserState BasicAst
whileExpr token = do
    condition <- expr
    consume [DO] "Expected 'do' after while condition."
    body <- decl
    consume [END] "Expected 'end' at the end of while."
    return . makeAst token $ While condition body

ifExpr :: Token -> ParserState BasicAst
ifExpr token = do
    condition <- expr
    consume [THEN] "Expected 'then' after if condition."
    body <- decl
    elseBody <- match [ELSE] (return Nothing) (const $ Just <$> decl)
    consume [END] "Expected 'end' at the end of the if then else declaration."

    return . makeAst token $ If { condition, body, elseBody }

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
    match [MINUS, NOT, NEW, BANG, PRINT, PRINTLN] call $
       \t -> makeAst t . Unary (value t) <$> unary

-- FIXME: fix this later
call :: ParserState BasicAst
call = do 
    func <- primary
    match [LEFT_PAREN, UNIT_VALUE, TRUE, FALSE, Num 0, Id ""] (return func) 
      $ \t -> do 
                modify (t:)
                arg <- call
                return $ case node arg of 
                              Call func' args -> makeAst (token arg) $ Call func (func':args)
                              _ -> makeAst (token func) $ Call func [arg]

primary :: ParserState BasicAst
primary = do
    token <- gets customHead
    modify customTail
    case value token of 
        LEFT_PAREN -> do
                res <- decl 
                token <- consume [RIGHT_PAREN] "Missing enclosing ')'."
                -- TODO: substitute token for parenthesis
                -- return res
                return $ Ast { node = node res, ctx = token }

        UNIT_VALUE -> return . makeAst token $ Unit
        TRUE       -> return . makeAst token $ Bool True
        FALSE      -> return . makeAst token $ Bool False
        (Num n)    -> return . makeAst token $ Number n
        (Id name)  -> return . makeAst token $ Var name

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
