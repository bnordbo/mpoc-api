{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Mpoc.Data
  ( runData
  -- * Pockets
  , addPocket
  , listPockets
  -- * Fragments
  , addFragment
  , getFragment
  , listFragments
  ) where

import           Control.Lens
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.AWS
import           Control.Monad.Trans.Control
import           Data.Conduit
import qualified Data.Conduit.List               as CL
import           Data.HashMap.Strict                (HashMap)
import qualified Data.HashMap.Strict             as Map
import           Data.Text                          (Text)
import           Mpoc.Types                  hiding (Env)
import           Network.AWS.DynamoDB
import qualified Data.UUID                       as UUID


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


--------------------------------------------------------------------------------
-- Pocket

addPocket :: Pocket -> Data ()
addPocket Pocket{..} = do
    env <- ask
    runResourceT . runAWST env . within Ireland . send $
        putItem "Pockets" & piItem .~ pocket
    return ()
  where
    pocket = Map.fromList
        [ ("UserId", attributeValue & avS .~ Just (userIdText user))
        , ("Name",   attributeValue & avS .~ Just name)
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
      [(":userId", attributeValue & avS .~ Just (userIdText uid))]


--------------------------------------------------------------------------------
-- Fragment

addFragment :: Fragment -> Data ()
addFragment Fragment{..} = do
    env <- ask
    runResourceT . runAWST env . within Ireland . send $
        putItem "Fragments" & piItem .~ fragment
    return ()
  where
    fragment = Map.fromList
        [ ("UserId",     attributeValue & avS .~ Just (userIdText userId))
        , ("FragmentId", attributeValue & avS .~ Just (fragIdText fragId))
        , ("Title",      attributeValue & avS .~ Just title)
        , ("Access",     attributeValue & avN .~ Just "0")
        , ("Body",       attributeValue & avS .~ Just body)
        ]

getFragment :: UserId -> FragmentId -> Data (Either DataError Fragment)
getFragment uid fid = do
    env <- ask
    runResourceT . runAWST env . within Ireland $
        decode <$> send (getItem "Fragments" & giKey .~ keys)
  where
    decode (view girsItem -> rs) =
      if Map.null rs then Left missing else fromDynamoDB rs
    missing = MissingItem $ "fragment for " ++ show uid ++ "/" ++ show fid
    keys = Map.fromList
        [ ("UserId",     attributeValue & avS .~ Just (userIdText uid))
        , ("FragmentId", attributeValue & avS .~ Just (fragIdText fid))
        ]

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
      [(":userId", attributeValue & avS .~ Just (userIdText uid))]


--------------------------------------------------------------------------------
-- Helpers

-- XXX: These are silly. There must be a type class I can use.
userIdText :: UserId -> Text
userIdText (UserId uuid) = UUID.toText uuid

fragIdText :: FragmentId -> Text
fragIdText (FragmentId uuid) = UUID.toText uuid
