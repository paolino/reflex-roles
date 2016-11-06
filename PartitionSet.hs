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

data Partition a = Partition [a] [a]

data PartitionCfg = PartitionCfg {
  updatingMessage :: Text
                                     }
instance Plugin Partition a where
  data Operation Partition a = Move a
  type Config Partition a = PartitionCfg
  type Use Partition a = (Eq a, PrettyShow a)
  operate (Move x) (Partition xs@(elem x -> True) ys) = Partition (delete x xs) (x:ys)
  operate (Move x) (Partition xs ys) = Partition (x:xs) (delete x ys)

updating cfg _ _ =  el "span" . text $ updatingMessage cfg

listening cfg xs@(Partition (sort -> ls) (sort -> rs)) = do
    ml <- divClass "left" $ fmap leftmost . el  "ul" . forM ls $ \x ->
          el "li" $ (x <$) <$> (button $ prettyShow $ x)

    rl <- divClass "right" $ fmap leftmost . el  "ul" . forM rs $ \x ->
            el "li" $ (x <$) <$> (button $ prettyShow $ x )

    return $ Move <$> leftmost [ml,rl]

runPartitionSetP :: (MS m, Use Partition a, PrettyShow a,Eq a, Ord a)
      => PartitionCfg
     -> (Operation Partition a -> m (ES (Maybe Text)))
      -> Partition a
     -> ES (Partition a)
     -> m (ES (Partition a))

runPartitionSetP partitionerCfg moveInState us refresh = runPipe
      (withExternal partitionerCfg listening updating moveInState)
      (Listening us) refresh
--