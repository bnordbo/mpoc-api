{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

import Control.Lens
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.AWS
import Control.Monad.Trans.Control
import Data.Conduit
import Mpoc.Types
import Network.AWS.Data
import Network.AWS.DynamoDB
import Network.AWS.Types
import System.IO


newtype Data a = Data { unwrap :: ReaderT Env IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader Env
           )

instance MonadBase IO Data where
  liftBase = liftIO

instance MonadBaseControl IO Data where
  type StM Data a = StM (ReaderT Env IO) a
  liftBaseWith f  = Data $ liftBaseWith $ \run -> f (run . unwrap)
  restoreM        = Data . restoreM

runData :: MonadIO m => Env -> Data a -> m a
runData e d = liftIO $ runReaderT (unwrap d) e

-- env' <- newEnv Ireland (FromProfile "mpoc_devel")
---

test :: Data [Fragment]
test = do
  env <- ask
  runResourceT . runAWST env . within Ireland $ do
    return []
