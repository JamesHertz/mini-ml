module Tests where

import qualified Data.Map as Map
import Data.Char (isDigit, isAlpha, isAlphaNum, isSpace)
import Control.Monad.State (State, evalState, get, modify, put, gets, MonadTrans (lift))
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
    -- single character
    | BANG
    -- keywords
    | TRUE
    | FALSE
    | LET
    | IN
    -- parenthesis
    | LEFT_PAREN
    | RIGHT_PAREN 
    -- values
    | Num Int
    | Id String
    -- special c:
    | EOF
 deriving (Show, Ord)


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
    BANG  == BANG  = True

    TRUE  == TRUE  = True
    FALSE == FALSE = True

    LEFT_PAREN  == LEFT_PAREN = True
    RIGHT_PAREN == RIGHT_PAREN = True 

    LET == LET = True
    IN  == IN = True
    EOF == EOF = True

    _ == _ = False


reservedKeywors :: Map.Map String TokenValue
reservedKeywors = Map.fromList [ 
     ("true", TRUE),
     ("false", FALSE),
     ("let", LET),
     ("in", IN)
    ]

data Token = Token { 
    value    :: TokenValue,
    line     :: Int, 
    position :: Int 
 } deriving (Show)

data Context = Context {
    source       :: String,
    currLine     :: Int,
    currPosition :: Int
  }

-- type ScannerState = State Context
type ScannerState = ExceptT Error (State Context)

tokenize :: String -> Result [Token]
tokenize src = evalState (runExceptT tokenize') $ Context src 1 (-1)

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

                   -- comparison
                   '>' -> match '=' GT_EQ GT' 
                   '<' -> match '=' LT_EQ LT'
                   '=' -> match '=' EQ_EQ EQ'
                   '!' -> match '=' N_EQ BANG

                   '|' -> consume '|' OR
                   '&' -> consume '&' AND

                   x | isAlpha x -> join x >>= identifier 
                   x | isDigit x -> join x >>= number
                   _   -> makeError $ "Unexpected symbol: " ++ [char]
        token <- makeToken value 
        rest  <- tokenize'
        return $ token : rest
        where
            -- joins char with the rest of the characters of the source c:
            join char = gets $ \(Context source _ _) -> char : source

advance :: ScannerState (Maybe Char)
advance = do
    state <- get
    case source state of
        []     -> return Nothing
        (x:xs) -> do
            updateCtx xs 0 1
            return $ Just x

makeToken :: TokenValue -> ScannerState Token
makeToken value = do
  state <- get
  return $ Token value (currLine state) (currPosition state)

match :: Char -> a -> a -> ScannerState a
match expected ifTrue ifFalse = do
    ctx <- get
    case source ctx of
        (x:xs) | x == expected -> do
            advance
            return ifTrue
        _ -> return ifFalse

consume :: Char -> TokenValue -> ScannerState TokenValue
consume expected result = do
  res <- match expected True False
  if res then return result
  else makeError $ "Expected a '" ++ [expected] ++ "'."

skipSpaces :: ScannerState ()
skipSpaces = do
  ctx <- get
  let 
    (spaces, rest) = span isSpace $ source ctx
    pos   = length spaces
    lines = length $ filter (== '\n') spaces

  updateCtx rest lines pos

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
    modify $ \(Context source line pos) -> Context newSource (line + lineDiff) (pos + posDiff)

makeError :: String -> ScannerState a
makeError msg = do
  ctx <- get
  throwE $ Error SyntaxError msg (currLine ctx) (currPosition ctx)