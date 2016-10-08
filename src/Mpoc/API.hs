{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Mpoc.API
  ( MpocAPI
  , app
  ) where

import Mpoc.API.Fragment
import Mpoc.API.Pocket
import Network.Wai.Handler.Warp
import Servant


type MpocAPI
  =    "pockets"   :> PocketAPI
  :<|> "fragments" :> FragmentAPI

server :: Server MpocAPI
server = pocketServer
    :<|> fragmentServer

mpocApi :: Proxy MpocAPI
mpocApi = Proxy

app :: Application
app = serve mpocApi server
