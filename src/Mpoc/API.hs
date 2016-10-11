{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Mpoc.API
  ( MpocAPI
  , mpocApi
  , mpocServer
  ) where

import Mpoc.API.Fragment
import Mpoc.API.Pocket
import Mpoc.Types
import Network.Wai.Handler.Warp
import Servant


type MpocAPI
  =    "pockets"   :> PocketAPI
  :<|> "fragments" :> FragmentAPI

mpocServer :: ServerT MpocAPI Mpoc
mpocServer = pocketServer
        :<|> fragmentServer

mpocApi :: Proxy MpocAPI
mpocApi = Proxy
