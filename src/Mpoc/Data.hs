{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.AWS
import Control.Monad.Reader
import Data.Conduit
import Mpoc.Types
import Network.AWS.Data
import Network.AWS.DynamoDB
import Network.AWS.Types
import System.IO


newtype Data a = Data { unwrap :: ReaderT AuthEnv IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader AuthEnv
           )

runData :: MonadIO m => AuthEnv -> Data a -> m a
runData e d = liftIO $ runReaderT (unwrap d) e

---

test :: Data [Fragment]
test = undefined
