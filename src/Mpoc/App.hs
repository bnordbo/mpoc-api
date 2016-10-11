{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module Mpoc.App (app) where

import Control.Monad.Except
import Control.Monad.Reader
import Mpoc.API
import Mpoc.Types
import Servant


mpocToHandler' :: forall a. Env -> Mpoc a -> Handler a
mpocToHandler' c m = ExceptT $ runReaderT (runExceptT (runMpoc m)) c

mpocToHandler :: Env -> Mpoc :~> Handler
mpocToHandler c = Nat (mpocToHandler' c)

mpocRunner :: Env -> Server MpocAPI
mpocRunner e = enter (mpocToHandler e) mpocServer

app :: Env -> Application
app = serve mpocApi . mpocRunner
