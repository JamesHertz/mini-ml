
module Types (
    module Types,
    Token(..),
    TokenValue(..)
) where
import Scanner (Token(..), TokenValue(..))
import Data.List (intercalate)

-- Major types
type TypedAst = Ast Type
type BasicAst = Ast Token

type GenId = String
-- supported types
data Type = 
      IntType 
    | BoolType 
    | UnitType 
    | RefType Type 
    | FuncType [Type] Type 
    | TypeVar GenId
    | FreeType [GenId] Type
    deriving (Eq, Ord)

instance Show Type where
    show IntType       = "int"
    show BoolType      = "bool"
    show UnitType      = "unit"
    show (RefType typ) = "ref " ++ show typ
    show (FuncType pars result) = intercalate " -> " $ map show pars ++ [show result]
    show (TypeVar x)   = "'" ++ x
    -- TODO: add ' in from of variable names c:
    show (FreeType vars typ) = "forall " ++ intercalate "," vars ++ " => " ++ show typ

type TypeToken   = Token
type TypeContext = (Type, TypeToken)

data Assigment a = Assigment {
    varName      :: String,
    expectedType :: Maybe TypeContext,
    assignValue  :: Ast a
} deriving (Show, Eq)

{-

AstNodes formally ...

-- Unary nodes
Unary MINUS    _
Unary NOT      _
Unary PRINTLN  _ 
Unary PRINT    _ 
Unary NEW      _ 
Unary BANG     _ 

-- binary nodes

-- arithmetic operators
Binary _ PLUS  _
Binary _ MINUS _
Binary _ TIMES _
Binary _ SLASH _

-- logical operators
Binary _ AND _
Binary _ OR  _

-- comparison operatos 
Binary _  GT'  _
Binary _  LT'  _
Binary _ GT_EQ _
Binary _ LT_EQ _
Binary _ EQ_EQ _
Binary _ N_EQ  _

-- special nodes

LetBlock _ _
RefAssignment _
If { condition, body, elsebody }
Sequence _ _ 
Call _ _ 
FuncDecl _ _ 

-- primary nodes
Number _ 
Bool   _ 
Var    _
Unit 

 -}

-- TODO: create a type that will encapsulate both a string and token
type Parameter = (String, Token, Maybe Type)
-- TODO: think about making this a functor (which will probably simplify typeCheck)
data Ast a     = Ast { ctx :: a, node :: AstNode a } deriving (Eq, Show)
data AstNode a = 
            Binary   (Ast a)  TokenValue (Ast a)
          | Unary    TokenValue (Ast a)
          | FuncDecl  [Parameter] (Ast a)
          | Call (Ast a) [Ast a]
          | PartialApplication (Ast a) [Ast a]
          | LetBlock [Assigment a] (Ast a)
          | RefAssignment (Ast a) (Ast a)
          | If { condition :: Ast a, body :: Ast a, elseBody :: Maybe (Ast a) }
          | Var      String
          | Number   Int
          | Bool     Bool
          | Sequence (Ast a) (Ast a)
          | While    (Ast a) (Ast a)
          | Unit
         deriving (Eq, Show)

-- useful functions
token :: BasicAst -> Token
token = ctx

type' :: TypedAst -> Type
type' = ctx


-- IDEAS FOR FUTURE
-- TODO: think about this...
-- having informations here
--
-- data BinaryOperation = 
-- -- arithmetic 
--       ADD 
--     | SUB 
--     | TIMES 
--     | DIV 
-- -- logical
--     | OR 
--     | AND 
-- -- comparison
--     | GT 
--     | LT 
--     | EQ
--     | N_EQ
--     | GT_E
--     | LT_E
--     deriving (Show, Eq)
--
--data UnaryOperation = 
--    | NEW
--    | INV
--    | NEG
--    | PRINT
--    | PRINTLN
-- TODO:: Change all case to maybes c:
