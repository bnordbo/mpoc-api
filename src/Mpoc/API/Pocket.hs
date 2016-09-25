{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Mpoc.API.Pocket
  ( PocketAPI
  , pocketServer
  ) where

import Data.Aeson                (FromJSON, ToJSON)
import Data.Text                 (Text)
import GHC.Generics
import Network.Wai
import Servant


data PocketAccess = Private | Public
  deriving (Eq, Show, Generic)

instance FromJSON PocketAccess
instance ToJSON PocketAccess

data Pocket = Pocket
  { name   :: String
  , access :: PocketAccess
  } deriving (Eq, Generic, Show)

instance FromJSON Pocket
instance ToJSON Pocket

type PocketAPI
  =    Get  '[JSON] [Pocket]
  :<|> ReqBody '[JSON] Pocket :> Post '[JSON] Pocket

pocketServer :: Server PocketAPI
pocketServer = listPockets :<|> addPocket
  where
    listPockets :: Handler [Pocket]
    listPockets = return pockets

    addPocket :: Pocket -> Handler Pocket
    addPocket = return

---

pockets = [Pocket "Foo" Private, Pocket "Bar" Private]
