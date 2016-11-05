{-# language TypeSynonymInstances, FlexibleInstances #-}
module NonTextual where


import Data.Text

class CanParse a where
  canParse :: Text -> Maybe a

instance CanParse Text where
  canParse = Just

instance CanParse String where
  canParse = Just . unpack

data NotTextual a = NotTextual {unNotTextual :: a}

instance Read a => CanParse (NotTextual a) where
  canParse t = case reads . unpack $ t of
    [(x,_)] -> Just $ NotTextual x
    _ -> Nothing

class PrettyShow a where
    prettyShow :: a -> Text

instance Show a => PrettyShow (NotTextual a) where
  prettyShow (NotTextual t) = pack $ show t

instance PrettyShow Text where
  prettyShow t = t

instance PrettyShow String where
  prettyShow t = pack  t

