{-# LANGUAGE TemplateHaskell, DeriveFunctor, ViewPatterns, RankNTypes, ImpredicativeTypes #-}

import Data.Graph.Inductive
import Data.Text (Text)
import Data.String
import Control.Lens.TH
import Control.Lens
import Data.Tuple
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid


data Concept = User Text | Role Text | Permission Text deriving (Show,Eq,Ord)

makePrisms ''Concept

data Knowledge = Knowledge {
  _graph :: Gr Concept (),
  _concepts :: M.Map Concept Node
  }

type Selector = APrism' Concept Text

knowledge  :: [LNode Concept] -> [LEdge ()] -> Knowledge
knowledge concepts relations = let
  in Knowledge
    (mkGraph concepts relations)
    (M.fromList $ map swap concepts)

query0 :: Selector -> Concept -> Knowledge -> [Text]
query0 t c k = toListOf (traverse . clonePrism t) $ let
    (Just (map snd -> ys,_,_, map snd -> xs),_) = match (concepts k M.! c) (graph k)
    resolve = map (fromJust . lab (graph k))
  in (resolve xs ++ resolve ys)

query :: [Selector] -> Concept -> Knowledge -> [[Concept]]
query [] _ _ = []
query (t:ts)  c k = let
  xs = map (view (re (clonePrism t))) . query0 t c $ k
  in xs:concatMap (\c -> query ts c k) xs

data Target =
  Link Concept Concept
  Vertex Concept

data Operation = Insert Target | Delete Target

modify :: Operation -> Knowledge -> Knowledge

    {-
insert :: Concept -> Concept -> Knowledge -> Knowledge
insert c1 c2 k = let

delete :: Concept ->
-}
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

k = knowledge cs rs
