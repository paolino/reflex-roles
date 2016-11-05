{-# LANGUAGE RecursiveDo, GADTs, InstanceSigs ,QuasiQuotes,FlexibleInstances,TemplateHaskell, TupleSections, ScopedTypeVariables, ConstraintKinds, FlexibleContexts, OverloadedStrings, OverlappingInstances, OverloadedLists#-}
import Reflex.Dom
import Prelude hiding ((.),id, lookup)
import Control.Monad (void,forM,forM_)
import Control.Lens (view,(.~),(&))
-- import Data.GADT.Compare
import Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import Data.GADT.Compare
import Data.Dependent.Map hiding (delete)-- (DMap,DSum( (:=>) ),singleton, lookup,fromList)
import Data.Text(Text,pack)
import Data.List (delete)
import Data.String.Here (here)
import Data.Time.Clock
import Control.Concurrent
import Control.Monad.Trans
import System.Random
import Data.Either
import Control.Monad.Identity
import qualified Data.Map as M
import Data.Map (Map)
import Data.Semigroup
import Control.Category
import Data.FileEmbed
import Lib
import Widgets
import Fake (fake)
import qualified DynamicList as Dl


maybeRight (Right x) = Just x
maybeRight _ = Nothing

type User = Text
type Role = Text
type Message = Text
                      --
type State = Map Role [User]

renderState :: MS m => State -> m (ES a)
renderState m = divClass "state" $ do
    forM_ (M.assocs m) $ \(k,us) -> do
      text k
      el "ul" $ forM_ us $ el "li" . text
    return never

data RolesMorph = Roling State | Roled State Role | Failed Text | Booting

data Interface m = Interface {
  -- add or remove a user from a role
  updateState :: Role -> Dl.Operation User -> m (ES (Maybe Text)),
  -- get a full state
  getState ::  m (ES (Either Text State))
  }

rolesW :: MS m
            => Interface m
            -> Source m RolesMorph State
rolesW (Interface updateState getState) = Source core where

    usersCfg = [Dl.BackButton:=> return "revoke", Dl.UpdatingMessage :=> return "updating server..."]
    usersW rs r = runPipe
      (Dl.dynamicList usersCfg (updateState r))
      (Dl.Listening (rs M.! r)) never

    core (Roling rs) = fmap rightG . divClass "roling" $ do
          fmap (fmap (Roled rs). leftmost) . el  "ul" . forM (M.keys rs) $ \w ->
                el "li" $ do
                  (w <$) <$> button w

    core (Roled rs r) = divClass "roled" $ do
        us <- do
            el "span" $ text r
            divClass "users" $ usersW rs r

        rs' <- foldDyn (M.insert r) rs us
        toRoles <- divClass "back" $ button "^ roles"
        return . merge $ [
          RightG :=> Roling <$> tagPromptlyDyn rs' toRoles,
          LeftG :=> tagPromptlyDyn rs' us
          ]

    core Booting = divClass "booting" $ do
        text "collecting roles state ... "
        e <- getState
        return . merge $ [
          RightG :=> either Failed Roling <$> e,
          LeftG :=> fmapMaybe maybeRight e
          ]

    core (Failed t) = divClass "failed" $ do
      text t
      rightG <$> (Booting <$) <$> button "retry"


css = $(embedStringFile "./roles.css")

---------------------  simulation ----------------------

roles = M.fromList [("Admins",["paolino","meditans"]),("Authors",["legolas","machupichu"]),
  ("Commenters",["blackfirst","zanzibar"]),("Users",["marco"])]
    :: State

fakeInterface :: MS m => Interface m
fakeInterface = Interface
  Dl.fakeUpdate
  (fake [Left "problem getting initial state", Right roles])



main = mainWidget $ do
  el "style" $ text $ css
  s <- divClass "operation" $ do
    runSource (rolesW fakeInterface) Booting
  _ <- divClass "log" $ do
    ss <- holdDyn mempty s
    domMorph renderState ss
  return ()

