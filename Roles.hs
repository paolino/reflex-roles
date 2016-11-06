{-# LANGUAGE RecursiveDo, GADTs, InstanceSigs ,QuasiQuotes,FlexibleInstances,TemplateHaskell, TupleSections, ScopedTypeVariables, ConstraintKinds, FlexibleContexts, OverloadedStrings,  OverloadedLists, ViewPatterns#-}

import Prelude hiding (lookup)
import Control.Monad (void,forM,forM_)
import Control.Lens (view,(.~),(&),over,ix, preview, _Right)
import Data.Dependent.Map (DSum( (:=>) ))
import Data.Text(Text)
import Data.List (delete, nub, (\\), sort)
import Data.Map (Map,(!),elems,insert,keys,fromList,assocs,findWithDefault)
import Data.FileEmbed (embedStringFile)
import Control.Arrow ((&&&))

import Fake (fake)
import DynamicList (runDynListP,DynList(..),DynamicListCfg (..))
import PartitionSet  (runPartitionSetP , Partition(..), PartitionCfg(..))
import Widgets (Source(..), runSource)
import ExternalPhase (Operation,fakeUpdate)

import Lib (MS,ES, domMorph, EitherG(LeftG,RightG), rightG)
import Reflex.Dom


type User = Text
type Role = Text
type Message = Text

type State = Map Role [User]

-- the bucket for all users
allUsers = "Users" :: Role

-- renders just the allUsers key
renderUsers :: (MS m) => State -> m (ES a)
renderUsers m = divClass "state" $ do
  elClass "span" "title" $ text allUsers
  el "ul" $ forM_ (sort $ findWithDefault [] allUsers m) $ el "li" . text
  return never

data RolesMorph = Roling State | Roled State Role | Failed Text | Booting

data Interface m = Interface {
  -- add or remove a user from a role
  addDelState :: Role -> Operation (DynList User) -> m (ES (Maybe Text)),
  -- flag a user in a role
  moveInState :: Role -> Operation (Partition User) -> m (ES (Maybe Text)),
  -- get a full state
  getState ::  m (ES (Either Text State))
  }

parts :: Role -> State -> Partition User
parts r rs = uncurry Partition . (id &&& (\\) (rs ! allUsers)) $ (rs ! r)

specialCase :: Role -> [User] -> State -> State
specialCase ((==) allUsers -> True) us = fmap $ filter (`elem` us)
specialCase _ us = over (ix allUsers) $ \us' -> us ++ (us' \\ us)


rolesW :: (MS m)
            => Interface m
            -> Source m RolesMorph State
rolesW (Interface addDelState moveInState getState) = Source core where

    usersCfg = DynamicListCfg "(revoke)" "updating server..."

    partitionerCfg = PartitionCfg "updating server..."

    core (Roling rs) = fmap rightG . divClass "roling" $ do
          fmap (fmap (Roled rs). leftmost) . el  "ul" . forM (keys rs) $ \w ->
                el "li" $ do
                  (w <$) <$> button w

    core (Roled rs r) = divClass "roled" $ do
        elClass "span" "title" $ text r
        toRoles <- divClass "back" $ button "\x2630"

        rec   us <- (unDynList <$>) <$> do
                    divClass "changer" $
                      runDynListP usersCfg (addDelState r) (DynList $
                        rs ! r) (DynList <$> flip (!) r <$> updated rs')

              ps <- if r /= allUsers then  divClass "mover" $
                runPartitionSetP partitionerCfg (moveInState r) (parts r rs) $
                        parts r <$> updated rs'
                    else return never

              rs' <- foldDyn (\(f,us) -> insert r us . f) rs $
                leftmost [(specialCase r &&& id) <$> us,
                          (const id &&& id) <$> (\(Partition xs _) -> xs) <$> ps
                         ]

        return . merge $ [
                RightG :=> Roling <$> tagPromptlyDyn rs' toRoles,
                LeftG :=> updated rs'
                ]

    core Booting = divClass "booting" $ do
        text "collecting roles state ... "
        e <- getState
        return . merge $ [
          RightG :=> either Failed Roling <$> e,
          LeftG :=> fmapMaybe (preview _Right) e
          ]


    core (Failed t) = divClass "failed" $ do
      text t
      rightG <$> (Booting <$) <$> button "retry"


css = $(embedStringFile "./roles.css")

---------------------  simulation ----------------------

roles =[("Admins",["paolino","meditans"]),("Authors",["legolas","machupichu"]),
  ("Commenters",["blackfirst","zanzibar","marco"])]
    :: [(Role,[User])]

fixRoles rs = fromList $ r:rs where
  r = (allUsers, nub . concat . map snd  $ rs)


fakeInterface :: MS m => Interface m
fakeInterface = Interface
  (const fakeUpdate)
  (const fakeUpdate)
  (fake [(1,Left "problem getting initial state"),(2, Right $ fixRoles roles)])


main = mainWidget $ do
  el "style" $ text $ css
  s <- divClass "operation" $ do
    runSource (rolesW fakeInterface) Booting
  _ <- divClass "log" $ do
    ss <- holdDyn mempty s --
    domMorph renderUsers ss
  return ()


