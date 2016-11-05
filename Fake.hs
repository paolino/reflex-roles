{-# language ConstraintKinds, FlexibleContexts, GADTs #-}
module Fake where

import Reflex.Dom
import Lib
import System.Random
import Control.Concurrent
import Control.Monad.Trans
import Control.Monad
import Data.Time.Clock


fake xs = getPostBuild >>= \e -> rest ((1,xs) <$e)

rest    :: MS m
        => ES (NominalDiffTime,[a]) -- ^ delay time in seconds + value
                -> m (ES a)
rest e =  performEventAsync . ffor e $ \(dt,as) cb -> liftIO . void . forkIO $ do
  threadDelay . ceiling $ dt * 1000000
  t <- randomIO
  cb $ as !! (t `mod` length as)

