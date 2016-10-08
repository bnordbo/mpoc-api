{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Mpoc.API.Pocket
  ( PocketAPI
  , pocketServer
  ) where

import Mpoc.Types
import Network.Wai
import Servant


type PocketAPI
  =    Get  '[JSON] [Pocket]
  :<|> ReqBody '[JSON] Pocket :> Post '[JSON] Pocket

pocketServer :: Server PocketAPI
pocketServer = listPockets :<|> addPocket
  where
    listPockets :: Handler [Pocket]
    listPockets = return []

    addPocket :: Pocket -> Handler Pocket
    addPocket = return
