{-# language ConstraintKinds, FlexibleContexts, GADTs #-}
module Fake where

import Reflex.Dom
import Lib
import System.Random
import Control.Concurrent
import Control.Monad.Trans
import Control.Monad
import Data.Time.Clock

consume ((n,x):xs) m
  | m < n = x
  | otherwise = consume xs (m - n)

fromDist xs = let
  s = sum . map fst $ xs
  in consume xs <$> randomRIO (0,s -1)


fake :: MS m => [(Int,a)] -> m (ES a)
fake xs = getPostBuild >>= \e -> rest ((1000,xs) <$e)

rest    :: MS m
        => ES (Int,[(Int,a)]) -- ^ delay time in seconds + value
                -> m (ES a)
rest e =  performEventAsync . ffor e $ \(dt,as) cb -> liftIO . void . forkIO $ do
  t <- randomRIO (0,dt*1000)
  threadDelay t
  fromDist as >>= cb

