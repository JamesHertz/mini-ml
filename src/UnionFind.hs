module UnionFind (
    TypedUnionFind,
    newRepr,
    lookupRepr,
    union,
    setReprType,
    empty
) where

import qualified Data.Map as Map
import Types (Type(..))
import Control.Monad.State (State, get, evalState)
import Data.Attoparsec.Text.Lazy (letter)
import Text.Printf (printf)

data Node = Parent Int Type | Child String deriving (Eq, Show)
-- Custom union find for type checking purporse
type TypedUnionFind = Map.Map String Node

type NodeId = String

-- TODO: make this more effient by shrinking size of this thing c:
empty = Map.empty

-- TODO: think about deletions ...
newRepr :: NodeId -> Type -> TypedUnionFind -> TypedUnionFind
newRepr node typ = Map.insert node $ Parent 0 typ

lookupRepr :: NodeId -> TypedUnionFind -> Maybe Type
lookupRepr node uf = 
    case Map.lookup node uf of 
         Just (Parent _ tp)  -> Just tp
         Just (Child parent) -> Just $ nodeType . snd . reprNode parent $ uf
         Nothing -> Nothing
  
union :: NodeId -> NodeId -> TypedUnionFind -> TypedUnionFind
union node1 node2 uf = 
  let 
      (id1, Parent h1 t1) = reprNode node1 uf
      (id2, Parent h2 t2) = reprNode node2 uf
      result1 =  Map.insert id2 (Child id1) uf
      result2 =  Map.insert id1 (Child id2) uf
  in 
     if h1 > h2      then result1
     else if h2 > h1 then result2
     else Map.insert id1 (Parent (h1 + 1) t1) result1

reprNode :: NodeId -> TypedUnionFind -> (String, Node)
reprNode nodeId uf = do 
    case Map.lookup nodeId uf of 
        Nothing   -> error $ "Error: Couldn't find entry: " ++ nodeId
        Just (Child parent) -> reprNode nodeId uf
        Just node -> (nodeId, node)

setReprType :: NodeId -> Type -> TypedUnionFind -> TypedUnionFind 
setReprType node typ uf = 
    case Map.lookup node uf of 
        Nothing -> error $ "Error: Couldn't find entry for: " ++ node
        Just (Parent h _) -> Map.insert node (Parent h typ) uf
        Just _  -> error $ printf "Error: Node '%s' is not a parent node!" node


-- func <$> Map.lookup node uf
--     where 
--         func (Parent _ typ) = typ
--         -- FIXME: later
--         func (Child parent) = nodeType $ evalState (nodeLookup parent) uf

-- nodeLookup :: String -> State TypedUnionFind Node
-- nodeLookup nodeId = do 
--     map <- get
--     case Map.lookup nodeId map of 
--         Nothing   -> error $ "Error: Couldn't find entry: " ++ nodeId
--         Just (Child parent) -> nodeLookup nodeId
--         Just node -> return node


hasNode :: String -> TypedUnionFind -> Bool
hasNode = Map.member

nodeType (Parent _ typ) = typ
-- nodeId  (Parent id _)   = id
