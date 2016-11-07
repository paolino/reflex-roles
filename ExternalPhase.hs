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

-- plugin logic
class  Plugin a  where
  data Operation a -- operations supported on (a)
  type Use a :: Constraint -- things possible on 'a'
  operate :: Use a => Operation a -> a -> a -- operation application

type family Config  a

--  widget to get operations
type ListeningP m a =  Config a  -> a -> m (ES (Operation a))
type UpdatingP m a = Config a  -> a -> Operation a -> m ()

data State a = Listening (a) | Updating (a) (Operation a) | Problem (a) Text

withExternal
  :: (MS m,  Plugin a, Use a)
  => Config a -- configuration
  -> ListeningP m a
  -> UpdatingP m a
  -> (Operation a -> m (ES (Maybe Text))) -- external operation, Just is error
  -> Pipe m (a) (a) (State a)


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

-- simulate an external update failing once on ten
fakeUpdate :: MS m => Operation a -> m (ES (Maybe Text))
fakeUpdate _ = fake [(1,Just "problem updating"),(10,Nothing)]





