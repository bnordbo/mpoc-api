{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

import           Control.Error
import           Control.Lens
import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.AWS
import           Control.Monad.Trans.Control
import           Data.Conduit
import qualified Data.Conduit.List           as CL
import           Data.HashMap.Strict            (HashMap)
import qualified Data.HashMap.Strict         as Map
import           Data.Text                      (Text)
import           Mpoc.Types
import           Network.AWS.Data
import           Network.AWS.DynamoDB
import           Network.AWS.Types
import           System.IO
import qualified Data.UUID                  as UUID


data DataError = BadFormat

newtype Data a = Data { unwrap :: ReaderT Env IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadCatch
           , MonadThrow
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

devEnv :: (MonadCatch m, MonadIO m) => m Env
devEnv = newEnv Ireland (FromFile "mpoc_devel" "/Users/bg/.aws/credentials")


--------------------------------------------------------------------------------
-- Pocket

addPocket :: Pocket -> Data ()
addPocket p = do
    env <- ask
    runResourceT . runAWST env . within Ireland $ send $
        putItem "Pockets" & piItem .~ pocket
    return ()
  where
    pocket = Map.fromList $
        [ ("UserId", attributeValue & avS .~ Just userText)
        , ("Name",   attributeValue & avS .~ Just (name p))
        , ("Access", attributeValue & avN .~ Just "0")
        ]
    userText = let (UserId uuid) = user p in
        UUID.toText uuid

listPockets :: Data [Pocket]
listPockets = undefined

toPocket :: HashMap Text AttributeValue -> Either DataError Pocket
toPocket avs = Pocket <$> user <*> title <*> access
  where
    user   = note BadFormat (Map.lookup "user" avs
                             >>= view avS
                             >>= fmap UserId . UUID.fromText)
    title  = note BadFormat (Map.lookup "title" avs >>= view avS)
    access = pure PrivatePocket


--------------------------------------------------------------------------------
-- Fragment

addFragment :: Fragment -> Data ()
addFragment f = undefined

listFragments :: Data [Fragment]
listFragments = undefined

getFragment :: FragmentId -> Data (Maybe Fragment)
getFragment fid = undefined

test :: Data [Maybe Fragment]
test = do
    env <- ask
    runResourceT . runAWST env . within Ireland $
        paginate (scan "Fragments")
        =$= CL.concatMap (view srsItems)
        =$= CL.map toFragment
        $$ CL.consume

toFragment :: HashMap Text AttributeValue -> Maybe Fragment
toFragment avs = Fragment <$> id <*> title <*> access <*> body
  where
    id     = Map.lookup "id" avs
             >>= view avS >>= fmap FragmentId . UUID.fromText
    title  = Map.lookup "title" avs >>= view avS
    access = pure PrivateFragment
    body   = Map.lookup "body" avs >>= view avS
