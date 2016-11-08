{-# language TemplateHaskell, MultiParamTypeClasses, FlexibleContexts, ViewPatterns, GADTs, OverloadedStrings, TypeFamilies, FlexibleInstances, OverloadedLists #-}

module PartitionSet (runPartitionSetP, Partition (..), PartitionCfg (..)) where

import Data.Text (Text,pack,unpack)
import Data.List (delete,sort)
import Data.Dependent.Map (DMap , DSum ((:=>)),fromList,(!))
import Data.GADT.Compare.TH
import Control.Monad (forM)
import Data.Maybe -- (isNothing)

import Widgets (Pipe(Pipe),runPipe)
import Lib
import Reflex.Dom
import Fake
import NonTextual
import ExternalPhase
import qualified Data.Set as S
import Data.Foldable

data Partition a = Partition (S.Set a) (S.Set a)

data PartitionCfg = PartitionCfg {
  updatingMessage :: Text
  }

type instance Config (Partition a) = PartitionCfg

instance Plugin (Partition a) where
  data Operation (Partition a) = Move a
  type Use (Partition a) = (Ord a, Eq a, PrettyShow a)
  operate (Move x) (Partition xs@(S.member x -> True) ys) = Partition (S.delete x xs) (S.insert x ys)
  operate (Move x) (Partition xs ys) = Partition (S.insert x xs) (S.delete x ys)

updating cfg xs@(Partition (toList -> ls) (toList -> rs)) _ = do
    divClass "left" $ el  "ul" . forM ls $ \x ->
          el "li" $ elAttr "button" [("disabled","")] $ text $ prettyShow $ x

    divClass "right" $ el  "ul" . forM rs $ \x ->
          el "li" $ elAttr "button" [("disabled","")] $ text $ prettyShow $ x
    return ()

listening cfg xs@(Partition (toList -> ls) (toList -> rs)) = do
    ml <- divClass "left" $ fmap leftmost . el  "ul" . forM ls $ \x ->
          el "li" $ (x <$) <$> (button $ prettyShow $ x)

    rl <- divClass "right" $ fmap leftmost . el  "ul" . forM rs $ \x ->
            el "li" $ (x <$) <$> (button $ prettyShow $ x )

    return $ Move <$> leftmost [ml,rl]

runPartitionSetP :: (MS m, Use (Partition a), PrettyShow a,Eq a, Ord a)
      => PartitionCfg
     -> (Operation (Partition a) -> m (ES (Maybe Text)))
      -> (Partition a)
     -> ES ((Partition a))
     -> m (ES ((Partition a)))

runPartitionSetP partitionerCfg moveInState us refresh = runPipe
      (withExternal partitionerCfg listening updating moveInState)
      (Listening us) refresh
--
