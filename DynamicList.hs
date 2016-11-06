{-# language MultiParamTypeClasses, TemplateHaskell, FlexibleContexts, ViewPatterns, GADTs, OverloadedStrings, TypeFamilies, FlexibleInstances, OverloadedLists, FunctionalDependencies, ConstraintKinds #-}

module DynamicList (DynList (..), runDynListP, DynamicListCfg (..)) where

import Data.Text (Text,pack,unpack)
import Data.List (delete,sort)
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
import Input

data DynamicListCfg = DynamicListCfg {
  elemSelect :: Text,
  updatingMessage :: Text
 }

type instance Config (DynList a) = DynamicListCfg

newtype DynList a = DynList {unDynList :: [a]}

instance Plugin (DynList a) where
  data Operation (DynList a) = Del a | Add a
  type Use (DynList a) = Eq a
  operate (Del x) (DynList xs) = DynList (delete x xs)
  operate (Add x) (DynList xs) = DynList $ x:xs

type WC a = (Show a, CanParse a, Eq a, PrettyShow a, Ord a)

listening   :: (MS m, WC a)
            => ListeningP m (DynList a)
listening cfg (DynList (sort -> xs)) = do
  add <- fmap canParse <$> resettable True Nothing -- to be fixed with a more serious input
  del <- fmap leftmost . el  "ul" . forM xs $ \x ->
        el "li" $ do
          el "span"  $ text (prettyShow $ x)
          (x <$) <$> (button $ elemSelect cfg)


  return $ leftmost [Del <$> del, Add <$> fromJust <$> ffilter isJust add]

updating :: (MS m, WC a) => UpdatingP m (DynList a)
updating cfg (DynList (sort -> xs)) _ =   do
  _ <- resettable False (Just $ updatingMessage cfg)-- to be fixed with a more serious input
  del <- el  "ul" . forM xs $ \x -> do
    el "li" $ do
      el "span"  $ text (prettyShow $ x)
      elAttr "button" [("disabled","")] $ text $ elemSelect cfg
  return ()
runDynListP
    :: (MS m, WC a)
    => DynamicListCfg
    -> (Operation (DynList a) -> m (ES (Maybe Text)))
    -> DynList a
    -> ES (DynList a)
    -> m (ES (DynList a))
runDynListP usersCfg addDelState us refresh = runPipe
      (withExternal usersCfg listening updating addDelState)
      (Listening us) refresh



