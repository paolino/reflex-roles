{-# LANGUAGE RecursiveDo, FlexibleContexts, ConstraintKinds #-}

module Widgets where

import Lib
import NonTextual
import Reflex.Dom
import Data.Dependent.Map
import Control.Monad.Identity

-- a source widget
newtype Source m s b = Source (s -> m (Cable (EitherG b s)))

runSource :: MS m => Source m s b -> s -> m (ES b)
runSource (Source w) s0 = do
  rec     r <- fan <$> domMorph w h
          h <- holdDyn s0 $ select r RightG
  return $ select r LeftG

-- an arrow widget
newtype Pipe m s a b = Pipe (Either a s -> m (Cable (EitherG b s)))

runPipe :: MS m => Pipe m s a b -> s -> ES a -> m (ES b)
runPipe (Pipe w) s0 ea = do
  rec     r <- fan <$> domMorph w h
          h <- holdDyn (Right s0) $ leftmost [Right <$> select r RightG, Left <$> ea]
  return $ select r LeftG


-- a sink widget
newtype Sink m s a = Sink (Either a s -> m (ES s))

runSink :: MS m => Sink m s a -> s -> ES a -> m ()
runSink (Sink w) s0 ea = do
  rec     r <- domMorph w h
          h <- holdDyn (Right s0) $ leftmost [Right <$> r, Left <$> ea]
  return ()

