{-# language TemplateHaskell, FlexibleContexts, ViewPatterns, GADTs, OverloadedStrings, TypeFamilies, FlexibleInstances, OverloadedLists #-}

module DynamicList where

import Data.Text (Text,pack,unpack)
import Data.List (delete)
import Data.Dependent.Map (DMap , DSum ((:=>)),fromList,(!))
import Data.GADT.Compare.TH
import Control.Monad (forM)
import Data.Maybe -- (isNothing)

import Widgets (Pipe(Pipe), Config)
import Lib
import Reflex.Dom
import Fake
import NonTextual

fakeUpdate _ _ = fake [Just "problem updating",Nothing]

data Operation a = Del a | Add a

operate (Del x) = delete x
operate (Add x) = (:) x

data State a = Listening [a] | Updating [a] (Operation a) | Problem [a] Text

data DynamicListCfg a where
  BackButton :: DynamicListCfg Text
  UpdatingMessage :: DynamicListCfg Text

deriveGEq ''DynamicListCfg
deriveGCompare ''DynamicListCfg

dynamicList  :: (MS m, Show a, CanParse a, Eq a, PrettyShow a)
        => DMap DynamicListCfg m -- text for delete button
        -> (Operation a -> m (ES (Maybe Text))) -- external operation, Just is error
        -> Pipe m (State a) [a] [a]

dynamicList  ((!) -> cfg)  external = Pipe core where

  core (Right (Listening xs)) = divClass "listening" $ do
    add <- fmap canParse <$> resettable -- to be fixed with a more serious input
    del <- fmap leftmost . el  "ul" . forM xs $ \x ->
          el "li" $ do
            el "span"  $ text (prettyShow $ x)
            (x <$) <$> (cfg BackButton >>= button)
    return . rightG $ Updating xs <$> leftmost [Del <$> del, Add <$> fromJust <$> ffilter isJust add]

  core (Right (Updating xs op)) = divClass "updating" $ do
    cfg UpdatingMessage >>= (el "span" . text)
    r <- external op
    let xs' = operate op xs
    return . merge $ [
        RightG :=> maybe (Listening xs') (Problem xs) <$> r ,
        LeftG :=> xs' <$ ffilter isNothing r
        ]
  core (Right (Problem xs t)) = divClass "problem" $ do
    el "span" $ text t
    b <- button "Got it"
    return $ rightG $ Listening xs <$ b

  core (Left xs) =  core $ Right (Listening xs)

