{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Mpoc.Types
    ( -- * Mpoc
      Env            (..)
    , Mpoc           (..)
    , UserId         (..)
    , PocketAccess   (..)
    , Pocket         (..)
    , FragmentAccess (..)
    , Fragment       (..)
    , FragmentId     (..)
    , NewFragment    (..)
      -- * Re-exports
    , module Mpoc.Data.Types
    ) where

import           Control.Error          (note)
import           Control.Monad          (mzero)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.HashMap.Strict as Map
import           Data.Text              (Text)
import           Data.UUID              (UUID)
import qualified Data.UUID           as UUID
import           GHC.Generics
import           Mpoc.Data.Types
import qualified Network.AWS.Env     as AWS
import           Servant                (ServantErr)
import           Web.HttpApiData        (FromHttpApiData(..))


--------------------------------------------------------------------------------
-- Mpoc

data Env = Env { aws :: AWS.Env }

newtype Mpoc a = Mpoc { runMpoc :: ExceptT ServantErr (ReaderT Env IO) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadError ServantErr
             , MonadIO
             , MonadReader Env
             )


--------------------------------------------------------------------------------
-- User

newtype UserId = UserId UUID
  deriving (Eq, Show)

instance FromHttpApiData UserId where
  parseQueryParam q =
      UserId <$> note "Invalid user ID" (UUID.fromText q)

instance FromJSON UserId where
  parseJSON = withText "uuid string" $
    maybe mzero (return . UserId) . UUID.fromText

instance ToJSON UserId where
  toJSON (UserId uuid) = String $ UUID.toText uuid


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

instance FromDynamoDB Pocket where
    fromDynamoDB avs =
        Pocket <$> attr avs "UserId" avS (fmap UserId . UUID.fromText)
               <*> attr avs "Name"   avS pure
               <*> attr avs "Access" avN (const $ pure PublicPocket)


--------------------------------------------------------------------------------
-- Fragment

data FragmentAccess = PrivateFragment | PublicFragment
  deriving (Eq, Show, Generic)

instance FromJSON FragmentAccess
instance ToJSON FragmentAccess

newtype FragmentId = FragmentId UUID
  deriving (Eq, Show)

instance FromHttpApiData FragmentId where
  parseQueryParam q =
      FragmentId <$> note "Invalid fragment ID" (UUID.fromText q)

instance FromJSON FragmentId where
  parseJSON = withText "uuid string" $
    maybe mzero (return . FragmentId) . UUID.fromText

instance ToJSON FragmentId where
  toJSON (FragmentId uuid) = String $ UUID.toText uuid

data Fragment = Fragment
    { userId :: UserId
    , fragId :: FragmentId
    , title  :: Text
    , access :: FragmentAccess
    , body   :: Text
    } deriving (Eq, Generic, Show)

instance ToJSON Fragment

instance FromDynamoDB Fragment where
    fromDynamoDB avs =
        Fragment <$> attr avs "UserId"     avS (fmap UserId . UUID.fromText)
                 <*> attr avs "FragmentId" avS (fmap FragmentId . UUID.fromText)
                 <*> attr avs "Title"      avS pure
                 <*> attr avs "Access"     avN (const $ pure PublicFragment)
                 <*> attr avs "Body"       avS pure

-- XXX: Maybe split the pure API types from the model types?
-- XXX: Do we set access immediately? How do we expose public fragments?
data NewFragment = NewFragment
    { userId :: UserId
    , title  :: Text
    , body   :: Text
    } deriving (Eq, Generic, Show)

instance FromJSON NewFragment
