
{-# LANGUAGE GADTs, ScopedTypeVariables, DataKinds, FlexibleContexts, Rank2Types, ConstraintKinds, TypeFamilies, MultiParamTypeClasses, FlexibleInstances, NoMonomorphismRestriction, RecursiveDo, InstanceSigs, OverloadedStrings #-}
module Lib where
import Prelude hiding ((.),id, lookup)
import Control.Monad (void,forM,forM_)
import Control.Lens (view,(.~),(&))
import Data.GADT.Compare
import Data.Dependent.Map hiding (delete)-- (DMap,DSum( (:=>) ),singleton, lookup,fromList)
import Data.Text(Text,pack)
import Data.Time.Clock
import Control.Concurrent
import Control.Monad.Trans
import Control.Monad
import System.Random
--import Data.Either
--import Control.Monad.Identity
--import qualified Data.Map as M
--import Data.Map (Map)
import Data.Semigroup
import Control.Category


import Data.Dependent.Map (DMap,DSum((:=>)), singleton)
import qualified Data.Dependent.Map as DMap
import Data.GADT.Compare (GCompare)
import Reflex hiding (combineDyn)
import qualified Reflex as Reflex
import Reflex.Dom hiding (combineDyn)
import GHC.Exts
import Control.Monad.Identity (Identity)
import qualified GHCJS.DOM.EventM as J
import Data.IORef
import Control.Monad.Trans



-------  reflex missings --------------
type Morph t m a = Dynamic t (m a) -> m (Event t a)

mapMorph  :: (MonadHold t m, Reflex t) => Morph t m (Event t b) -> (a -> m (Event t b)) -> Dynamic t a -> m (Event t b)
mapMorph dyn f d = dyn (f <$> d) >>= joinE

joinE :: (Reflex t, MonadHold t f) => Event t (Event t a) -> f (Event t a)
joinE = fmap switch . hold never

pick :: (GCompare k, Reflex t) => k a -> Event t (DMap k Identity) -> Event t a
pick x r = select (fan r) x -- shouldn't we share fan r ?

gateWith f = attachWithMaybe $ \allow a -> if f allow then Just a else Nothing

pair x = leftmost . (: [x])


------------- Spider synonims

type ES = Event Spider
type DS = Dynamic Spider
type BS = Behavior Spider

-------------- Dom + spider synonyms

type MS = MonadWidget Spider
type Plug a = ES (DMap a Identity)

-- specialized mapMorph for the Dom host
domMorph ::     MonadWidget t m
                => (a -> m (Event t b))  -- widget rewriter
                -> Dynamic t a           -- driver for rewritings
                -> m (Event t b)         -- signal the happened rewriting
domMorph = mapMorph dyn

-------------- create a Plug ----------
mergeDSums :: GCompare a => [DSum a ES] -> Plug a
mergeDSums = merge . DMap.fromList

wire' :: (GCompare k) => k v -> ES v -> Plug k
wire' x = merge . singleton x

wire (k :=> v) = wire' k v
------------- Lib -------------------------------------
instance GCompare k => Semigroup (DMap k Identity) where
  (<>) = union
-- | Enter enabled input widget, just emit  the contents on return

resettable :: MS m => m (ES Text)
resettable = do
  rec     let resettable = def & setValue .~ ("" <$) enter
              value  = current . view textInput_value $ input
              enter  = value `tag` keypress Enter input
          input <- textInput resettable
  return enter

-- | delay an Event by the amount of time specified in its value and random
-- pick from chances
--



-- polymorphic 2-keys type DSum
data EitherG r l a where
  RightG :: EitherG l r r
  LeftG ::  EitherG l r l

instance GEq (EitherG r l) where
  RightG `geq` RightG = Just Refl
  LeftG `geq` LeftG = Just Refl
  _ `geq` _ = Nothing

instance GCompare (EitherG r l) where
  RightG `gcompare` LeftG = GLT
  LeftG `gcompare` RightG = GGT
  RightG `gcompare` RightG = GEQ
  LeftG `gcompare` LeftG = GEQ

rightG b = wire (RightG :=> b)
leftG b = wire (LeftG :=> b)



instance GCompare k => IsList (DMap k f) where
  type Item (DMap k f) = DSum k f
  fromList = Data.Dependent.Map.fromList
  toList = Data.Dependent.Map.toList
