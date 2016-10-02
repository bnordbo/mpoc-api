{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

import           Control.Lens
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
import           Network.AWS.DynamoDB
import qualified Data.UUID                  as UUID


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
    pocket = Map.fromList
        [ ("UserId", attributeValue & avS .~ Just (userText $ user p))
        , ("Name",   attributeValue & avS .~ Just (name p))
        , ("Access", attributeValue & avN .~ Just "0")
        ]

listPockets :: UserId -> Data [Either DataError Pocket]
listPockets uid = do
    env <- ask
    runResourceT . runAWST env . within Ireland $
        paginate (query "Pockets"
                  & qKeyConditionExpression    .~ condition
                  & qExpressionAttributeValues .~ attributes)
        =$= CL.concatMap (view qrsItems)
        =$= CL.map fromDynamoDB
         $$ CL.consume
  where
    condition  = Just "UserId = :userId"
    attributes = Map.fromList
      [(":userId", attributeValue & avS .~ Just (userText uid))]


--------------------------------------------------------------------------------
-- Fragment

addFragment :: Fragment -> Data ()
addFragment f = undefined

getFragment :: FragmentId -> Data (Maybe Fragment)
getFragment fid = undefined

listFragments :: UserId -> Data [Either DataError Fragment]
listFragments uid = do
    env <- ask
    runResourceT . runAWST env . within Ireland $
        paginate (query "Fragments"
                  & qKeyConditionExpression    .~ condition
                  & qExpressionAttributeValues .~ attributes)
        =$= CL.concatMap (view qrsItems)
        =$= CL.map fromDynamoDB
        $$ CL.consume
  where
    condition  = Just "UserId = :userId"
    attributes = Map.fromList
      [(":userId", attributeValue & avS .~ Just (userText uid))]


--------------------------------------------------------------------------------
-- Helpers

userText :: UserId -> Text
userText (UserId uuid) = UUID.toText uuid
