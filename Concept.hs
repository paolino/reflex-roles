
{-# language TemplateHaskell,OverloadedStrings, NoMonomorphismRestriction, ViewPatterns, LambdaCase #-}

module Concept (example, roles, users, roleUsers, Concept (..) ,_User,_Role,_Permission, allUsers, setUsers, insertUser,
  setPermissions, insertPermission,rolePermissions,allPermissions,permissions
               ) where

import Data.Text (Text)
import Control.Lens.TH
import Control.Lens
import Model
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad

import Data.Graph.Inductive

allUsers = "qn489d4m78934dtmn389dt78mn7d84mn27349"
allPermissions = allUsers
eitherAllUsers ((==) allUsers -> True) = Left allUsers
eitherAllUsers x = Right x
eitherAllPermissions ((==) allPermissions -> True) = Left allPermissions
eitherAllPermissions x = Right x
----------------- example -------------------------------------------
data Concept = User Text | Role (Either Text Text) | Permission Text deriving (Show,Eq,Ord)

_User = prism User $ \x -> case x of
                              User y -> Right y
                              x -> Left x

_Role = prism f g where
  f ((==) allUsers -> True) = Role (Left allUsers)
  f x = Role (Right x)
  g (Role (Left u)) = Right u
  g (Role (Right u)) = Right u
  g x = Left x

_Permission = prism Permission $ \x -> case x of
                              Permission y -> Right y
                              x -> Left x


-- makePrisms ''Concept
cs :: [LNode Concept]
cs =  [
                (1,Role (Right "Account administrator")),
                (2,Role (Right "Product administrator")),
                (3,Role (Right "Product editor")),
                (4,Permission "View product"),
                (5,Permission "Edit product textual content"),
                (6,Permission "Edit product properties"),
                (7,Permission "Edit product price"),
                (8,Permission "Edit product images"),

                (9,User "admin@mydomain.com"),
                (10,User "otheradmin@mydomain.com"),
                (11,User "yetanotheradmin@mydomain.com"),
                (12,User "user1@mydomain.com"),
                (13,User "user2@mydomain.com"),
                (14,User "user3@mydomain.com"),
                (15,User "user4@mydomain.com"),
                (16,User "user7@mydomain.com")
      ]

rs = [          (1,9, ()),
                (1,10, ()),
                (1,11, ()),
                (2,12, ()),
                (2,13, ()),
                (2,14, ()),
                (3,15, ()),
                (3,14, ()),
                (3,16, ()),
                (4,1, ()),
                (5,1, ()),
                (1,6, ()),
                (1,7, ()),
                (1,8, ()),
                (2,4, ()),
                (2,5, ()),
                (2,6, ()),
                (2,7, ()),
                (3,4, ()),
                (3,5, ()),
                (3,8, ())
                ]

fixboot :: Concept -> Concept -> Maybe Bool
fixboot u@(User _) r@(Role _) = return False
fixboot r@(Role _) u@(User _) = return True
fixboot u@(Permission _) r@(Role _) = return False
fixboot r@(Role _) u@(Permission _) = return True
fixboot _ _ = Nothing

makeKC :: [LNode Concept] -> [LEdge ()] -> Maybe (Knowledge Concept)
makeKC cs rs = do
  let k = knowledge cs rs
      cs' = k ^. concepts
  fs <- forM rs $ \(n,m,()) -> do
    nl <- lab (k ^. graph) n
    ml <- lab (k ^. graph ) m
    (<$> fixboot nl ml) $ \case
      False -> id
      True -> delEdge (n,m) .  insEdge (m,n,())
  let k' = over graph (\k -> foldr ($) k fs) k
      us = users k
      ps = permissions k
      r = Role (Left allUsers)
      usk = foldr (\u -> modify $ Insert (Link (User u) r)) (modify (Insert (Vertex r)) k') $ us
      psk = foldr (\u -> modify $ Insert (Link (Permission u) r)) usk $ ps
  return $ psk

roles :: Knowledge Concept -> [Text]
roles = flatten _Role

users :: Knowledge Concept -> S.Set Text
users = S.fromList . flatten _User

permissions = S.fromList . flatten _Permission

roleUsers x =  S.fromList . toListOf (traverse . _User) . head . query [_User] (Role $ eitherAllUsers x)
rolePermissions x =  S.fromList . toListOf (traverse . _Permission) . head . query [_Permission] (Role $ eitherAllUsers x)

insertUser :: Text -> Knowledge Concept -> Knowledge Concept
insertUser u k =  let
  aus = users k
  in ($ k) $ case u `S.member` aus of
      False -> let
        adduser = modify (Insert (Vertex (User u))) -- no problem
        addlink  = modify (Insert (Link (User u) (Role (Left allUsers))))
          in addlink . adduser
      True -> id

setUsers :: Text -> S.Set Text -> Knowledge Concept -> Knowledge Concept
setUsers r@((==) allUsers -> True) us k = let
  aus = roleUsers r k
  neg = foldr (\u -> modify (Delete (Vertex (User u)))) k $ aus S.\\ us
  in foldr (\u -> insertUser u) neg $ us S.\\ aus
setUsers r us k = let
  aus = roleUsers r k
  pre = foldr (\u -> modify (Insert (Link (User u) (Role $ Right r))) . insertUser u) k $ us S.\\ aus
  in foldr (\u -> modify (Delete (Link (User u) (Role $ Right r)))) pre $ aus S.\\ us

insertPermission :: Text -> Knowledge Concept -> Knowledge Concept
insertPermission u k =  let
  aus = users k
  in ($ k) $ case u `S.member` aus of
      False -> let
        adduser = modify (Insert (Vertex (Permission u))) -- no problem
        addlink  = modify (Insert (Link (Permission u) (Role (Left allPermissions))))
          in addlink . adduser
      True -> id

setPermissions :: Text -> S.Set Text -> Knowledge Concept -> Knowledge Concept
setPermissions r@((==) allPermissions -> True) us k = let
  aus = rolePermissions r k
  neg = foldr (\u -> modify (Delete (Vertex (Permission u)))) k $ aus S.\\ us
  in foldr (\u -> insertPermission u) neg $ us S.\\ aus

setPermissions r us k = let
  aus = rolePermissions r k
  pre = foldr (\u -> modify (Insert (Link (Permission u) (Role $ Right r))) . insertPermission u) k $ us S.\\ aus
  in foldr (\u -> modify (Delete (Link (Permission u) (Role $ Right r)))) pre $ aus S.\\ us



userRoles :: Text -> Knowledge Concept -> [[Concept]]
userRoles x = query [_Role] (User x)

userPrermissions :: Text  -> Knowledge Concept -> [[Concept]]
userPrermissions x = query [_Role,_Permission] (User x)

Just example = makeKC cs rs
