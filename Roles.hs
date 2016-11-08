{-# LANGUAGE RecursiveDo, GADTs, InstanceSigs ,QuasiQuotes,FlexibleInstances,TemplateHaskell, TupleSections, ScopedTypeVariables, ConstraintKinds, FlexibleContexts, OverloadedStrings,  OverloadedLists, ViewPatterns#-}

import Prelude hiding (lookup)
import Control.Monad (void,forM,forM_)
import Control.Lens (view,(.~),(&),over,ix, preview, _Right)
--import Data.Dependent.Map (DSum( (:=>) ))
import Data.Dependent.Map hiding (insert,(!),keys,(\\), findWithDefault, map,fromList, toList)
import Data.Text(Text, pack)
import Data.List (delete, nub, (\\), sort)
import Data.Map (Map,(!),elems,insert,keys,fromList,assocs,findWithDefault)
import Data.FileEmbed (embedStringFile)
import Control.Arrow ((&&&))

import Fake (fake)
import DynamicList (runDynListP,DynList(..),DynamicListCfg (..))
import PartitionSet  (runPartitionSetP , Partition(..), PartitionCfg(..))
import Widgets (Source(..), runSource)
import ExternalPhase (Operation,fakeUpdate)
import Control.Monad.Identity

import Lib (MS,ES,DS, Message, domMorph, EitherG(LeftG,RightG), rightG,leftG, Cable,sselect)
import Reflex.Dom hiding (Delete, Insert, Link)
import Model hiding (Operation)
import Data.Set (Set)
import qualified Data.Set as S
---- app specific --
import Concept
import Data.Foldable
import Debug.Trace

type User = Text
type Role = Text


type State = Knowledge Concept

-- parts :: Role -> State -> Partition User
parts elems roleElems r k = let
  us =  roleElems r k
  in Partition us (elems k S.\\ us)



-- renders just the allUsers key
renderUsers :: (MS m) => State -> m (ES a)
renderUsers k = divClass "state" $ do
  elClass "span" "title" $ text "User list"
  trace (show $ users k) $ el "ul" $ forM_ (toList $ users k) $ el "li" . text
  return never

data RolesMorph = Roling State | Roled State Role | Editing State Role | Failed Text | Booting

data Interface m = Interface {
  -- add or remove a user from a role
  addDelState :: Role -> Operation (DynList User) -> m (ES (Maybe Text)),
  -- flag a user in a role
  moveInState :: Role -> Operation (Partition User) -> m (ES (Maybe Text)),
  -- get a full state
  getState ::  m (ES (Either Text State))
  }

fixTitleUsers r = if r == allUsers then "Users" else r
fixTitlePermissions r = if r == allPermissions then "Permissions" else r

usersCfg r = DynamicListCfg
  (if r == allUsers then "(delete)" else "(revoke)")
  "updating server..."

permissionsCfg r = DynamicListCfg
  (if r == allPermissions then "(delete)" else "(remove)")
  "updating server..."
data Phase = Editor | Roler
rolesW :: (MS m)
            => DS Phase
            -> Interface m
            -> Source m (Message (EitherG State Phase)) RolesMorph
rolesW phase (Interface addDelState moveInState getState) = Source core where


    partitionerCfg = PartitionCfg "updating server..."
    core (Editing rs r) = divClass "editing" $ do
        let partings = parts permissions rolePermissions r
        toRoles <- divClass "back" $ button "\x2630"
        edit <- fmap (Roler <$) . divClass "title" $ button (fixTitlePermissions r)
        rec   us <- (unDynList <$>) <$> do
                    divClass "changer" $
                      runDynListP (permissionsCfg r) (addDelState r) (DynList $
                        rolePermissions r rs) (DynList <$> rolePermissions r <$> updated rs')

              ps <- if r /= allPermissions then  divClass "mover" $
                runPartitionSetP partitionerCfg (moveInState r) (partings rs) $
                  partings <$> updated rs'
                    else return never

              rs' <- foldDyn (setSub (rolePermissions,permissions,Permission,allPermissions) r) rs $
                leftmost [us,(\(Partition xs _) -> xs) <$> ps]
        return . merge $ [
                RightG :=> leftmost [Roling <$> tagPromptlyDyn rs' toRoles,
                  attachPromptlyDynWith Roled rs' $ r <$ edit],
                LeftG :=> leftmost [leftG (updated rs'),rightG edit]
                ]


    core (Roling rs) = fmap rightG . divClass "roling" $ do
          let rephase Editor = Editing rs
              rephase Roler = Roled rs

          fmap (attachWith rephase (current phase) . leftmost) .
            el  "ul" . forM (toList $ roles rs) $ \w ->
                el "li" $ do
                  (w <$) <$> button (fixTitlePermissions w)


    core (Roled rs r) = divClass "roled" $ do
        let partings = parts users roleUsers r
        toRoles <- divClass "back" $ button "\x2630"
        edit <- fmap (Editor <$) . divClass "title" $ button (fixTitleUsers r)
        rec   us <- (unDynList <$>) <$> do
                    divClass "changer" $
                      runDynListP (usersCfg r) (addDelState r) (DynList $
                        roleUsers r rs) (DynList <$> roleUsers r <$> updated rs')

              ps <- if r /= allUsers then  divClass "mover" $
                runPartitionSetP partitionerCfg (moveInState r) (partings rs) $
                  partings  <$> updated rs'
                    else return never

              rs' <- foldDyn (setSub (roleUsers,users,User,allUsers) r) rs $
                leftmost [us,(\(Partition xs _) -> xs) <$> ps]
        return . merge $ [
                RightG :=> leftmost [Roling <$> tagPromptlyDyn rs' toRoles,
                  attachPromptlyDynWith Editing rs' $ r <$  edit],
                LeftG :=> leftmost [leftG (updated rs'),rightG edit]
                ]

    core Booting = divClass "booting" $ do
        text "collecting roles state ... "
        e <- getState
        return . merge $ [
          RightG :=> either Failed Roling <$> e,
          LeftG :=> leftG (fmapMaybe (preview _Right) e)
          ]


    core (Failed t) = divClass "failed" $ do
      text t
      rightG <$> (Booting <$) <$> button "retry"


css = $(embedStringFile "./roles.css")


---------------------  simulation ----------------------


fakeInterface :: MS m => Interface m
fakeInterface = Interface
  (const fakeUpdate)
  (const fakeUpdate)
  (fake [(1,Left "problem getting initial state"),(2, Right example)])


main = mainWidget $ do
  el "style" $ text $ css
  s <- divClass "operation" $ do
          rec ph <- holdDyn Roler (sselect RightG s)
              s <- fan <$> runSource (rolesW ph fakeInterface) Booting
          return s
  _ <- divClass "log" $ do
    ss <- holdDyn mempty $ sselect LeftG s
    domMorph renderUsers $ ss
    -- dynText $ (pack . show) <$> ss
  return ()

