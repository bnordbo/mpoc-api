{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Mpoc.API.Fragment
  ( FragmentAPI
  , fragmentServer
  ) where

import           Control.Applicative       ((<$>))
import           Control.Monad.IO.Class
import           Data.List                 (find)
import           Data.UUID.V4              (nextRandom)
import qualified Mpoc.Data              as Data
import           Mpoc.Types
import           Servant


type FragmentAPI
    =    Capture "userId" UserId         :> Get  '[JSON] [Fragment]
    :<|> Capture "fragmentId" FragmentId :> Get  '[JSON] Fragment
    :<|> ReqBody '[JSON] NewFragment     :> Post '[JSON] Fragment

fragmentServer :: ServerT FragmentAPI Mpoc
fragmentServer
    =    listFragments
    :<|> getFragment
    :<|> addFragment
  where
    addFragment :: NewFragment -> Mpoc Fragment
    addFragment nf = do
      i <- FragmentId <$> liftIO nextRandom
      let af = Fragment { userId = userId (nf :: NewFragment)
                        , fragId = i
                        , title  = title (nf :: NewFragment)
                        , access = PrivateFragment
                        , body   = body (nf :: NewFragment)
                        }
      Data.runData undefined $ Data.addFragment af
      return af

    listFragments :: UserId -> Mpoc [Fragment]
    listFragments uid = pure []

    getFragment :: FragmentId -> Mpoc Fragment
    getFragment n = do
      f <- pure []
      maybe (throwError $ err404 { errBody = "No such fragment" }) return
        $ find ((== n) . fragId) f
