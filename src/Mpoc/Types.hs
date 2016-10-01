{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Mpoc.Types where

import Control.Monad (mzero)
import Data.Aeson    (FromJSON(..), ToJSON(..), Value(String), withText)
import Data.Text     (Text)
import Data.UUID     (UUID, fromText, toText)
import GHC.Generics


--------------------------------------------------------------------------------
-- User

newtype UserId = UserId UUID
  deriving (Eq, Show)

instance FromJSON UserId where
  parseJSON = withText "uuid string" $
    maybe mzero (return . UserId) . fromText

instance ToJSON UserId where
  toJSON (UserId uuid) = String $ toText uuid


--------------------------------------------------------------------------------
-- Pocket

data PocketAccess = PrivatePocket | PublicPocket
  deriving (Eq, Show, Generic)

instance FromJSON PocketAccess
instance ToJSON PocketAccess

data Pocket = Pocket
    { user   :: UserId
    , name   :: Text
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

instance FromJSON FragmentId where
  parseJSON = withText "uuid string" $
    maybe mzero (return . FragmentId) . fromText

instance ToJSON FragmentId where
  toJSON (FragmentId uuid) = String $ toText uuid

data Fragment = Fragment
    { fragId :: FragmentId
    , title  :: Text
    , access :: FragmentAccess
    , body   :: Text
    } deriving (Eq, Generic, Show)

instance ToJSON Fragment

-- XXX: Maybe split the pure API types from the model types?
-- XXX: Do we set access immediately? How do we expose public fragments?
data NewFragment = NewFragment
    { title  :: String
    , body   :: String
    } deriving (Eq, Generic, Show)

instance FromJSON NewFragment
