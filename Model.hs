{-# LANGUAGE TemplateHaskell, DeriveFunctor, ViewPatterns,OverloadedStrings #-}
-- | A graph model for shared client server knowledge (state)
-- We use the expressiveness of fgl, without edge labels.
-- Totally type unsafe. In particular there is no control on the existance of
-- non sensical edges
--
module Model (Knowledge,Selector,Operation(..),Target(..),query,modify, flatten,knowledge, graph,concepts) where

import Data.Text (Text)
import Data.Maybe (fromJust)
import Control.Lens.TH (makeLenses, makePrisms)
import Control.Lens (clonePrism, APrism',re, view, toListOf)
import Data.Tuple (swap)
import qualified Data.Map as M
import Data.Graph.Inductive (empty,Gr,LNode,Node,LEdge,lab,match,newNodes,
  delNode,delEdge,insNode,insEdge,mkGraph)

data Knowledge a = Knowledge {
  _graph :: Gr a (), -- only considering vertices, knowledge is spreaded on
  _concepts :: M.Map a Node -- internal resolution on names to Node
                             } deriving Show

instance Ord a => Monoid (Knowledge a) where
  mempty = Knowledge empty mempty
  Knowledge g1 cs1 `mappend` Knowledge g2 cs2  = error "think about it"
makeLenses ''Knowledge

type Selector a = APrism' a Text

-- | build a knowledge from vertices and edges
knowledge  :: Ord a => [LNode a] -> [LEdge ()] -> Knowledge a
knowledge concepts relations = let
  in Knowledge
    (mkGraph concepts relations)
    (M.fromList $ map swap concepts)

query0 :: Ord a => Selector a -> a -> Knowledge a -> [Text]
query0 t c (Knowledge g cs) = toListOf (traverse  . clonePrism t) $ let
    (Just (map snd -> ys,_,_, map snd -> xs),_) = match (cs M.! c) g
    resolve = map (fromJust . lab g)
  in (resolve xs ++ resolve ys)

-- | query is performed on a bfs base. Each selector retrieve a level of knowledge
-- from the starting vertex. Each selector filter out the result by applying the
--  Prism as a   filter (more, much more to understand, before saying)
query :: Ord a => [Selector a] -> a -> Knowledge a -> [[a]]
query [] _ _ = []
query (t:ts)  c k = let
  xs = map (view (re (clonePrism t))) . query0 (clonePrism t) c $ k
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
-- flatten :: Selector a -> Knowledge a -> [Text]
flatten t (Knowledge _ cs) = toListOf (traverse .  t) $ M.keys cs


