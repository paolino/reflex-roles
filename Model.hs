{-# LANGUAGE TemplateHaskell, DeriveFunctor, ViewPatterns,OverloadedStrings #-}
-- | A graph model for shared client server knowledge (state)
-- We use the expressiveness of fgl, without edge labels.
-- Totally type unsafe. In particular there is no control on the existance of
-- non sensical edges
--
module Model (Knowledge,Selector,Operation(..),Target(..),query,modify) where

import Data.Text (Text)
import Data.Maybe (fromJust)
import Control.Lens.TH (makeLenses, makePrisms)
import Control.Lens (clonePrism, APrism',re, view, toListOf)
import Data.Tuple (swap)
import qualified Data.Map as M
import Data.Graph.Inductive (Gr,LNode,Node,LEdge,lab,match,newNodes,
  delNode,delEdge,insNode,insEdge,mkGraph)

data Knowledge a = Knowledge {
  _graph :: Gr a (), -- only considering vertices, knowledge is spreaded on
  _concepts :: M.Map a Node -- internal resolution on names to Node
                             } deriving Show

makeLenses ''Knowledge

type Selector a = APrism' a Text

-- | build a knowledge from vertices and edges
knowledge  :: Ord a => [LNode a] -> [LEdge ()] -> Knowledge a
knowledge concepts relations = let
  in Knowledge
    (mkGraph concepts relations)
    (M.fromList $ map swap concepts)

query0 :: Ord a => Selector a -> a -> Knowledge a -> [Text]
query0 t c (Knowledge g cs) = toListOf (traverse . clonePrism t) $ let
    (Just (map snd -> ys,_,_, map snd -> xs),_) = match (cs M.! c) g
    resolve = map (fromJust . lab g)
  in (resolve xs ++ resolve ys)

-- | query is performed on a bfs base. Each selector retrieve a level of knowledge
-- from the starting vertex. Each selector filter out the result by applying the
--  Prism as a   filter (more, much more to understand, before saying)
query :: Ord a => [Selector a] -> a -> Knowledge a -> [[a]]
query [] _ _ = []
query (t:ts)  c k = let
  xs = map (view (re (clonePrism t))) . query0 t c $ k
  in xs:concatMap (\c -> query ts c k) xs

-- |  target of a modification
data Target a = Link a a | Vertex a

-- | operation to be performed on a target
data Operation a = Insert (Target a) | Delete (Target a)

-- | interpret a modification as a change of knowledge
modify :: Ord a => Operation a -> Knowledge a -> Knowledge a
modify (Insert (Vertex c)) k@(Knowledge g cs) = case M.lookup c cs of
        Nothing -> let
                [n] = newNodes 1 g
             in Knowledge  (insNode (n,c) g) (M.insert c n cs)
        _ -> k -- failing silently

modify (Delete (Vertex c)) k@(Knowledge g cs)  = case M.lookup c cs of
        Just n -> Knowledge  (delNode n g) (M.delete c cs)
        _ -> k -- failing silently

modify (Insert (Link c1 c2)) k@(Knowledge g cs) =
          case (,) <$> M.lookup c1 cs <*> M.lookup c2 cs of
            Nothing -> k
            Just (n1,n2) -> Knowledge (insEdge (n1,n2,()) g) cs

modify (Delete (Link c1 c2)) k@(Knowledge g cs) = case (,) <$> M.lookup c1 cs <*> M.lookup c2 cs of
            Nothing -> k
            Just (n1,n2) -> Knowledge (delEdge (n1,n2) g) cs

-- | retrieve a class of the knowledge
flatten :: Selector a -> Knowledge a -> [Text]
flatten t (Knowledge _ cs) = toListOf (traverse .  clonePrism t) $ M.keys cs
----------------- example -------------------------------------------
data Concept = User Text | Role Text | Permission Text deriving (Show,Eq,Ord)

makePrisms ''Concept

cs :: [LNode Concept]
cs =  [
                (1,Role "Account administrator"),
                (2,Role "Product administrator"),
                (3,Role "Product editor"),
                (4,Permission "View product"),
                (5,Permission "Edit product textual content"),
                (6,Permission "Edit product properties"),
                (7,Permission "Edit product price"),
                (8,Permission "Edit product images"),

                (9,User "admin@mydomain.com"),
                (10,User "otheradmin@mydomain.com"),
                (11,User "yetanotheradmin@mydomain.com"),
                (12,User "user1@mydomain.com"),
                (13,User "user2@mydomain.com"),
                (14,User "user3@mydomain.com"),
                (15,User "user4@mydomain.com"),
                (16,User "user7@mydomain.com")
      ]

rs :: [LEdge ()]
rs = [          (1,9, ()),
                (1,10, ()),
                (1,11, ()),
                (2,12, ()),
                (2,13, ()),
                (2,14, ()),
                (3,15, ()),
                (3,14, ()),
                (3,16, ()),
                (1,4, ()),
                (1,5, ()),
                (1,6, ()),
                (1,7, ()),
                (1,8, ()),
                (2,4, ()),
                (2,5, ()),
                (2,6, ()),
                (2,7, ()),
                (3,4, ()),
                (3,5, ()),
                (3,8, ())
                ]
example :: Knowledge Concept
example = knowledge cs rs

roles :: Knowledge Concept -> [Text]
roles = flatten _Role

users :: Knowledge Concept -> [Text]
users = flatten _User

userRoles :: Text -> Knowledge Concept -> [[Concept]]
userRoles x = query [_Role] (User x)

userPrermissions :: Text  -> Knowledge Concept -> [[Concept]]
userPrermissions x = query [_Role,_Permission] (User x)

