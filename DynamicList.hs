{-# language MultiParamTypeClasses, TemplateHaskell, FlexibleContexts, ViewPatterns, GADTs, OverloadedStrings, TypeFamilies, FlexibleInstances, OverloadedLists, FunctionalDependencies #-}

module DynamicList where

import Data.Text (Text,pack,unpack)
import Data.List (delete)
import Data.Dependent.Map (DMap , DSum ((:=>)),fromList,(!))
import Data.GADT.Compare.TH
import Data.GADT.Compare
import Control.Monad (forM)
import Data.Maybe -- (isNothing)

import Widgets (Pipe(Pipe), runPipe)
import Lib
import Reflex.Dom
import Fake
import NonTextual
import GHC.Exts
import ExternalPhase

data DynamicListCfg = DynamicListCfg {
  backButton :: Text,
  updatingMessage :: Text
                                     }


newtype DynList a = DynList {unDynList :: [a]}

instance Plugin DynList a where
  data Operation DynList a = Del a | Add a
  type Config DynList a = DynamicListCfg
  type Use DynList a = (Show a, CanParse a, Eq a, PrettyShow a)
  operate (Del x) (DynList xs) = DynList (delete x xs)
  operate (Add x) (DynList xs) = DynList $ x:xs

listening cfg (DynList xs) = do
    add <- fmap canParse <$> resettable -- to be fixed with a more serious input
    del <- fmap leftmost . el  "ul" . forM xs $ \x ->
          el "li" $ do
            el "span"  $ text (prettyShow $ x)
            (x <$) <$> (button $ backButton cfg)
    return $ leftmost [Del <$> del, Add <$> fromJust <$> ffilter isJust add]

updating cfg _ _ = el "span" . text $ updatingMessage cfg

runDynListP
    :: (MS m, Show a, Eq a, PrettyShow a, CanParse a)
    => DynamicListCfg
    -> (Operation DynList a -> m (ES (Maybe Text)))
    -> DynList a
    -> ES (DynList a)
    -> m (ES (DynList a))
runDynListP usersCfg addDelState us refresh = runPipe
      (withExternal usersCfg listening updating addDelState)
      (Listening us) refresh



