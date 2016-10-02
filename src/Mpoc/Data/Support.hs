module Mpoc.Data.Support
    ( DataError    (..)
    , FromDynamoDB (..)
    , ToDynamoDB   (..)
    , attr
    -- * Re-exports
    , avS
    , avN
    ) where

import           Control.Error           (note)
import           Control.Exception
import           Control.Lens
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.Monoid             ((<>))
import           Data.Text               (Text)
import qualified Data.Text            as Text
import qualified Data.UUID            as UUID
import           Network.AWS.DynamoDB


data DataError
    = MissingAttribute String
    | BadType String
    | BadFormat String
    deriving Show

instance Exception DataError

class FromDynamoDB a where
    fromDynamoDB :: HashMap Text AttributeValue -> Either DataError a

class ToDynamoDB a where
    toDynamoDB :: a -> (Text, AttributeValue)

attr :: HashMap Text AttributeValue
     -> Text
     -> Getting (Maybe a) AttributeValue (Maybe a)
     -> (a -> Maybe b)
     -> Either DataError b
attr avs fa ta c =
     noteMissing (Map.lookup fa avs)
        >>= noteBadType . view ta
        >>= noteBadFormat . c
  where
    noteMissing   = note (MissingAttribute (attr <> " missing"))
    noteBadType   = note (BadType ("bad type for " <> attr))
    noteBadFormat = note (BadFormat ("invalid format for " <> attr))

    attr = Text.unpack fa
