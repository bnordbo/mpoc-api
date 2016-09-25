{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Mpoc.API.Pocket
  ( PocketAPI
  , pocketServer
  ) where

import Data.Aeson                (FromJSON, ToJSON)
import Data.List                 (find)
import Data.Maybe                (fromMaybe)
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
  :<|> Capture "pocketId" String :> Get  '[JSON] Pocket
  :<|> ReqBody '[JSON] Pocket :> Post '[JSON] Pocket

pocketServer :: Server PocketAPI
pocketServer = listPockets
    :<|> getPocket
    :<|> addPocket
  where
    addPocket :: Pocket -> Handler Pocket
    addPocket p = return p

    listPockets :: Handler [Pocket]
    listPockets = return pockets

    getPocket :: String -> Handler Pocket
    getPocket n = maybe (throwError $ err404 { errBody = "No such pocket" }) return
                $ find ((== n) . name) pockets

pockets = [Pocket "Foo" Private, Pocket "Bar" Private]
