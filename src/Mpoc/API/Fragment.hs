{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Mpoc.API.Fragment
  ( FragmentAPI
  , fragmentServer
  ) where

import           Control.Applicative           ((<$>))
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Data.List                     (find)
import           Data.UUID.V4                  (nextRandom)
import qualified Mpoc.Data                  as Data
import           Mpoc.Types
import           Servant


-- XXX: The user ID needs to be passed in.
type FragmentAPI
    =    Get  '[JSON] [Fragment]
    :<|> Capture "fragmentId" FragmentId :> Get  '[JSON] Fragment
    :<|> ReqBody '[JSON] NewFragment     :> Post '[JSON] Fragment

fragmentServer :: ServerT FragmentAPI Mpoc
fragmentServer
    =    listFragments
    :<|> getFragment
    :<|> addFragment
  where
    listFragments :: Mpoc [Fragment]
    listFragments = do
        e <- ask
        d <- Data.runData (aws e) (Data.listFragments undefined)
        either (const $ throwError err500) pure $ sequence d

    getFragment :: FragmentId -> Mpoc Fragment
    getFragment fid = do
        e <- ask
        f <- Data.runData (aws e) $ Data.getFragment undefined fid
        maybe (throwError $ err404 { errBody = "No such fragment" }) return
            $ find ((== fid) . fragId) f

    addFragment :: NewFragment -> Mpoc Fragment
    addFragment nf = do
        e <- ask
        i <- FragmentId <$> liftIO nextRandom
        let af = Fragment { userId = userId (nf :: NewFragment)
                          , fragId = i
                          , title  = title (nf :: NewFragment)
                          , access = PrivateFragment
                          , body   = body (nf :: NewFragment)
                          }
        Data.runData (aws e) $ Data.addFragment af
        pure af
