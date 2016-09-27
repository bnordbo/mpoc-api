{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Mpoc.Types where

import Control.Monad (mzero)
import Data.Aeson    (FromJSON(..), ToJSON(..), Value(String), withText)
import Data.UUID     (UUID, fromText, toText)
import GHC.Generics


--------------------------------------------------------------------------------
-- Pocket

data PocketAccess = PrivatePocket | PublicPocket
  deriving (Eq, Show, Generic)

instance FromJSON PocketAccess
instance ToJSON PocketAccess

data Pocket = Pocket
  { name   :: String
  , access :: PocketAccess
  } deriving (Eq, Generic, Show)

instance FromJSON Pocket
instance ToJSON Pocket

--------------------------------------------------------------------------------
-- Fragment

data FragmentAccess = PrivateFragment | PublicFragment
  deriving (Eq, Show, Generic)

instance FromJSON FragmentAccess
instance ToJSON FragmentAccess

newtype FragmentId = FragmentId UUID
  deriving (Eq, Show)

instance ToJSON FragmentId where
  toJSON (FragmentId uuid) = String $ toText uuid

instance FromJSON FragmentId where
  parseJSON = withText "uuid string" $
    maybe mzero (return . FragmentId) . fromText

data Fragment = Fragment
  { fragId :: FragmentId
  , title  :: String
  , access :: FragmentAccess
  , body   :: String
  } deriving (Eq, Generic, Show)

instance ToJSON Fragment

-- XXX: Maybe split the pure API types from the model types?
-- XXX: Do we set access immediately? How do we expose public fragments?
data NewFragment = NewFragment
  { title  :: String
  , body   :: String
  } deriving (Eq, Generic, Show)

instance FromJSON NewFragment
