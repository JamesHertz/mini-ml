module Scanner (
    TokenValue(..),
    Token(..),
    tokenize
) where

import qualified Data.Map as Map
import Data.Char (isDigit, isAlpha, isAlphaNum, isSpace)
import Control.Monad.State (State, evalState, get, modify, put, gets, MonadTrans (lift))
import Errors
import Data.Maybe (fromMaybe)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Except (throwE)
import Text.XHtml (start)

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
    -- single character
    | NOT   -- tild (~)
    | COLON -- (:)
    -- keywords
    | TRUE
    | FALSE
    | LET
    | IN
    | IF
    | THEN
    | ELSE
    | END
    | BOOL
    | INT
    | UNIT
    -- parenthesis
    | LEFT_PAREN
    | RIGHT_PAREN 
    -- values
    | Num Int
    | Id String
    -- special c:
    | EOF
 deriving (Show, Ord)


-- FIXME: find a better solution for this (ask the Mrs Haskell)
instance Eq TokenValue where
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

    NOT   == NOT   = True
    COLON == COLON = True

    TRUE  == TRUE  = True
    FALSE == FALSE = True

    IF   == IF   = True
    THEN == THEN = True
    ELSE == ELSE = True
    END  == END  = True

    BOOL == BOOL = True
    INT  == INT  = True
    UNIT == UNIT = True

    LEFT_PAREN  == LEFT_PAREN = True
    RIGHT_PAREN == RIGHT_PAREN = True 

    LET == LET = True
    IN  == IN  = True
    EOF == EOF = True

    _ == _ = False


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
     ("int", INT),
     ("bool", BOOL),
     ("unit", UNIT)
    ]

data Token = Token { 
    value    :: TokenValue,
    line     :: Int, 
    position :: Int 
 } deriving (Show, Eq)

-- TODO: save the start of a line and update it whenever you make a new token c:
data Context = Context {
    source        :: String,
    currLine      :: Int,
    currPosition  :: Int,
    startPosition :: Int
  }

-- type ScannerState = State Context
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
                   ':' -> return COLON

                   -- comparison
                   '>' -> match '=' GT_EQ GT' 
                   '<' -> match '=' LT_EQ LT'
                   '=' -> match '=' EQ_EQ EQ'
                   -- '!' -> match '=' N_EQ BANG -- FIXME: uncomment me in the Future c:

                   '!' -> consume '=' N_EQ "Expected '=' after '!'." -- FIXME: delete me in the future c:
                   '|' -> consume '|' OR  "Expected another '|'."
                   '&' -> consume '&' AND "Expected another '&'."

                   x | isAlpha x -> join x >>= identifier 
                   x | isDigit x -> join x >>= number
                   _   -> makeError $ "Unexpected symbol: " ++ [char]
        token <- makeToken value 
        rest  <- tokenize'
        return $ token : rest
        where
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
    (word, rest) = span isAlphaNum txt
    positions    = length word - 1 -- because one char was already consumed by the caller
    value        = fromMaybe (Id word) $ Map.lookup word reservedKeywors
  updateCtx rest 0 positions
  return value

-- why don't I need a lift??
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
