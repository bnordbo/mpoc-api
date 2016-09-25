{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators         #-}

module Mpoc.API.Fragment
  ( FragmentAPI
  , fragmentServer
  ) where

import Control.Applicative    ((<$>))
import Control.Monad          (mzero)
import Control.Monad.IO.Class
import Data.Aeson             (FromJSON(..), ToJSON(..), Value(String), withText)
import Data.List              (find)
import Data.Text              (Text)
import Data.UUID              (UUID, fromText, toText)
import Data.UUID.V4           (nextRandom)
import GHC.Generics
import Network.Wai
import Servant


data FragmentAccess = Private | Public
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

type FragmentAPI
  =    Get  '[JSON] [Fragment]
  :<|> Capture "fragmentId" FragmentId :> Get  '[JSON] Fragment
  :<|> ReqBody '[JSON] NewFragment :> Post '[JSON] Fragment

fragmentServer :: Server FragmentAPI
fragmentServer
    =    listFragments
    :<|> getFragment
    :<|> addFragment
  where
    addFragment :: NewFragment -> Handler Fragment
    addFragment f = do
      i <- FragmentId <$> liftIO nextRandom
      return Fragment
        { fragId = i
        , title  = title (f :: NewFragment)
        , access = Private
        , body   = body (f :: NewFragment)
        }

    listFragments :: Handler [Fragment]
    listFragments = liftIO fragments

    getFragment :: FragmentId -> Handler Fragment
    getFragment n = do
      f <- liftIO fragments
      maybe (throwError $ err404 { errBody = "No such fragment" }) return
        $ find ((== n) . fragId) f

---

fragments :: IO [Fragment]
fragments =
  mapM (\(t, a, b) -> do
           id <- FragmentId <$> nextRandom
           return $ Fragment id t a b)
    [ ("Lorem ipsum", Private, "Lorem ipsum dolor sit amet.")
    , ("Froob?", Private, "Foo. Foo hoo? Foo bar.")
    , ("Ur", Public, "Han kom som ett yrväder.")
  ]
