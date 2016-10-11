{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Mpoc.API.Pocket
  ( PocketAPI
  , pocketServer
  ) where

import Mpoc.Types
import Servant


type PocketAPI
    =    Get  '[JSON] [Pocket]
    :<|> ReqBody '[JSON] Pocket :> Post '[JSON] Pocket

pocketServer :: ServerT PocketAPI Mpoc
pocketServer = listPockets :<|> addPocket
  where
    listPockets :: Mpoc [Pocket]
    listPockets = return []

    addPocket :: Pocket -> Mpoc Pocket
    addPocket = return
