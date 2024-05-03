module Scanner (
    TokenValue(..),
    Token(..),
    tokenize, 
) where

import qualified Data.Map as Map
import Data.Char (isDigit, isAlpha, isAlphaNum, isSpace)
import Control.Monad.State (State, evalState, get, modify, put, gets )
import Errors
import Data.Maybe (fromMaybe)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Except (throwE)

data TokenValue = 
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
    -- ref assigment
    | ASSIGN
    -- single character
    | NOT   -- tild (~)
    | COLON -- (:)
    | SEMI_COLON
    | BANG  -- (!)
    -- keywords
    | TRUE
    | FALSE
    | LET
    | IN
    | IF
    | THEN
    | ELSE
    | END
    | PRINT
    | PRINTLN
    | NEW
    | WHILE
    | DO
    -- types c:
    | BOOL
    | INT
    | UNIT
    | REF
    -- parenthesis
    | LEFT_PAREN
    | RIGHT_PAREN 
    -- values
    | Num Int
    | Id String
    -- special c:
    | EOF
 deriving (Show, Ord, Eq)

data Token = Token { 
    value    :: TokenValue,
    line     :: Int, 
    position :: Int 
 } deriving (Show, Eq)

reservedKeywors :: Map.Map String TokenValue
reservedKeywors = Map.fromList [ 
     ("true", TRUE),
     ("false", FALSE),

     ("let", LET),
     ("in", IN),
     ("if", IF),
     ("then", THEN),
     ("else", ELSE),
     ("end", END),
     ("while", WHILE),
     ("do", DO),

     ("int", INT),
     ("bool", BOOL),
     ("unit", UNIT),

     ("print", PRINT),
     ("println", PRINTLN),

     -- references tokens
     ("ref", REF),
     ("new", NEW)
    ]

data Context = Context {
    source        :: String,
    currLine      :: Int,
    currPosition  :: Int,
    startPosition :: Int
  }

type ScannerState = ExceptT Error (State Context)

tokenize :: String -> Result [Token]
tokenize source = evalState (runExceptT tokenize') $ Context {
        source,
        currLine      = 1,
        currPosition  = 0,
        startPosition = 0
    }

tokenize' :: ScannerState [Token]
tokenize' = do
    skipSpaces

    next <- advance
    let func = flip $ maybe (do
          token <- makeToken EOF
          return [token]
         ) 

    func next $ \char -> do
        value <- case char of
                   -- arithmetic
                   '+' -> return PLUS
                   '-' -> return MINUS
                   '*' -> return TIMES
                   '/' -> return SLASH

                   '(' -> return LEFT_PAREN
                   ')' -> return RIGHT_PAREN

                   -- other single chars
                   '~' -> return NOT
                   ';' -> return SEMI_COLON

                   -- comparison & refs tokens
                   '>' -> match '=' GT_EQ GT' 
                   '<' -> match '=' LT_EQ LT'
                   '=' -> match '=' EQ_EQ EQ'
                   '!' -> match '=' N_EQ BANG
                   ':' -> match '=' ASSIGN COLON

                   '|' -> consume '|' OR  "Expected another '|'."
                   '&' -> consume '&' AND "Expected another '&'."

                   x | isAlpha x || x == '_' 
                       -> join x >>= identifier 
                   x | isDigit x -> join x >>= number
                   _   -> makeError "Unexpected symbol."
        token <- makeToken value 
        rest  <- tokenize'
        return $ token : rest
        where
            -- FIXME: find a better name for this function
            -- joins char with the rest of the characters of the source c:
            join char = gets $ \Context { source } -> char : source

advance :: ScannerState (Maybe Char)
advance = do
    ctx <- get
    case source ctx of
        []     -> return Nothing
        (x:xs) -> do
            updateCtx xs 0 1
            return $ Just x

makeToken :: TokenValue -> ScannerState Token
makeToken value = do
  state <- get
  updateStarPos
  return $ Token value (currLine state) (startPosition state)
  where 
    updateStarPos = modify $ 
        \ Context { source, currLine, currPosition } -> Context {
            source, currLine, currPosition, startPosition = currPosition
        }

match :: Char -> a -> a -> ScannerState a
match expected ifTrue ifFalse = do
    ctx <- get
    case source ctx of
        (x:xs) | x == expected -> do
            advance
            return ifTrue
        _ -> return ifFalse

consume :: Char -> TokenValue -> String -> ScannerState TokenValue
consume expected result msg = do
  res <- match expected True False
  if res then return result
  else makeError msg

skipSpaces :: ScannerState ()
skipSpaces = do
  ctx <- get
  let 
    (spaces, rest) = span isSpace $ source ctx
    pos   = length spaces
    lines = length $ filter (== '\n') spaces

  updateCtxSpaces rest lines pos
  where 
    updateCtxSpaces newSource lineDiff posDiff = 
        modify $ \ Context { currLine , currPosition } -> 
               Context { 
                    source        = newSource,
                    currLine      = currLine + lineDiff,
                    currPosition  = currPosition + posDiff,
                    startPosition = currPosition + posDiff
               }

number :: String -> ScannerState TokenValue
number txt = do
  let 
    (nr, rest) = span isDigit txt
    positions  = length nr - 1 -- because one char was already consumed by the caller
  updateCtx rest 0 positions
  return $ Num (read nr)

identifier :: String -> ScannerState TokenValue
identifier txt = do
  let 
    (word, rest) = span isIdentChar txt
    positions    = length word - 1 -- because one char was already consumed by the caller
    value        = fromMaybe (Id word) $ Map.lookup word reservedKeywors
  updateCtx rest 0 positions
  return value
  where 
    isIdentChar x = x == '_' || isAlphaNum x

updateCtx :: String -> Int -> Int -> ScannerState ()
updateCtx newSource lineDiff posDiff = 
    modify $ \ Context { currLine, currPosition, startPosition } -> Context { 
          source        = newSource,
          currLine      = currLine + lineDiff,
          currPosition  = currPosition + posDiff,
          startPosition 
        }

makeError :: String -> ScannerState a
makeError msg = do
  ctx <- get
   -- -1 because we are always of by 1 since we start currPosition at 0 (but still call advance)
  throwE $ Error SyntaxError msg (currLine ctx) (currPosition ctx - 1)
