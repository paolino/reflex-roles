{-# language MultiParamTypeClasses, TemplateHaskell, FlexibleContexts, ViewPatterns, GADTs, OverloadedStrings, TypeFamilies, FlexibleInstances, OverloadedLists, RankNTypes, FunctionalDependencies #-}

module ExternalPhase where

import Data.Text (Text,pack,unpack)
import Data.List (delete)
import Data.Dependent.Map (DMap , DSum ((:=>)),fromList,(!))
import Data.GADT.Compare.TH
import Data.GADT.Compare
import Control.Monad (forM)
import Data.Maybe -- (isNothing)

import Widgets (Pipe(Pipe))
import Lib
import Reflex.Dom
import Fake
import NonTextual
import GHC.Exts

fakeUpdate :: MS m => Operation r a -> m (ES (Maybe Text))
fakeUpdate _ = fake [(1,Just "problem updating"),(10,Nothing)]


type family Env (r :: * -> *) a :: * -> *

class  Plugin r a  where
  data Operation r a -- operation supported
  type Config r a
  type Use r a :: Constraint -- things possible on 'a'
  operate :: Use r a => Operation r a -> r a -> r a -- operation application
  --  widget to get operations, the config is the 'a'

type ListeningP m r a=  Config r a  -> r a -> m (ES (Operation r a))
type UpdatingP m r a = Config r a  -> r a -> Operation r a -> m ()

data State r a = Listening (r a) | Updating (r a) (Operation r a) | Problem (r a) Text

withExternal
  :: (MS m,  Plugin r a, Use r a)
  => Config r a -- configuration
  -> ListeningP m r a
  -> UpdatingP m r a
  -> (Operation r a -> m (ES (Maybe Text))) -- external operation, Just is error
  -> Pipe m (State r a) (r a) (r a)

withExternal cfg listening updating external = Pipe core where

  core (Right (Listening xs)) = fmap rightG . divClass "listening" $ (Updating xs <$>) <$> listening cfg xs

  core (Right (Updating xs op)) = divClass "updating" $ do
    updating cfg xs op
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



