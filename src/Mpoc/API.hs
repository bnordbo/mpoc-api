{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Mpoc.API where

import Mpoc.API.Fragment
import Mpoc.API.Pocket
import Network.Wai.Handler.Warp
import Servant


type MpocAPI = "pockets" :> PocketAPI

server :: Server MpocAPI
server = pocketServer

mpocApi :: Proxy MpocAPI
mpocApi = Proxy

app :: Application
app = serve mpocApi server

main :: IO ()
main = run 8081 app
