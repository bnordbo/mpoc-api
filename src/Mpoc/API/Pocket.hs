{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Mpoc.API.Pocket where

import Data.Aeson                (FromJSON, ToJSON)
import Data.List                 (find)
import Data.Maybe                (fromMaybe)
import Data.Text                 (Text)
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
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
  =    "pockets" :> Get '[JSON] [Pocket]
  :<|> "pockets" :> Capture "pocketId" String
                 :> Get '[JSON] Pocket
  :<|> "pockets" :> ReqBody '[JSON] Pocket
                 :> Post '[JSON] Pocket

---

server :: Server PocketAPI
server = listPockets
    :<|> singlePocket
    :<|> addPocket

  where
    addPocket :: Pocket -> Handler Pocket
    addPocket p = return p

    listPockets :: Handler [Pocket]
    listPockets = return pockets1

    singlePocket :: String -> Handler Pocket
    singlePocket n = maybe (throwError $ err404 { errBody = "No such pocket" }) return
                   $ find ((== n) . name) pockets1

mpocApi :: Proxy PocketAPI
mpocApi = Proxy

app1 :: Application
app1 = serve mpocApi server

pockets1 = [Pocket "Foo" Private, Pocket "Bar" Private]

main :: IO ()
main = run 8081 app1
