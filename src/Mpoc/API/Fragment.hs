{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators         #-}

module Mpoc.API.Fragment
  ( FragmentAPI
  , fragmentServer
  ) where

import Control.Applicative    ((<$>))
import Control.Monad.IO.Class
import Data.List              (find)
import Data.UUID.V4           (nextRandom)
import Mpoc.Types
import Network.Wai
import Servant


type FragmentAPI
  =    Get  '[JSON] [Fragment]
  :<|> Capture "fragmentId" FragmentId :> Get  '[JSON] Fragment
  :<|> ReqBody '[JSON] NewFragment :> Post '[JSON] Fragment

fragmentServer :: Server FragmentAPI
fragmentServer
    =    listFragments
    :<|> getFragment
    :<|> addFragment
  where
    addFragment :: NewFragment -> Handler Fragment
    addFragment f = do
      i <- FragmentId <$> liftIO nextRandom
      return Fragment
        { fragId = i
        , title  = title (f :: NewFragment)
        , access = Private
        , body   = body (f :: NewFragment)
        }

    listFragments :: Handler [Fragment]
    listFragments = liftIO fragments

    getFragment :: FragmentId -> Handler Fragment
    getFragment n = do
      f <- liftIO fragments
      maybe (throwError $ err404 { errBody = "No such fragment" }) return
        $ find ((== n) . fragId) f

---

fragments :: IO [Fragment]
fragments =
  mapM (\(t, a, b) -> do
           id <- FragmentId <$> nextRandom
           return $ Fragment id t a b)
    [ ("Lorem ipsum", PrivateFragment, "Lorem ipsum dolor sit amet.")
    , ("Froob?", PrivateFragment, "Foo. Foo hoo? Foo bar.")
    , ("Ur", PublicFragment, "Han kom som ett yrväder.")
  ]
