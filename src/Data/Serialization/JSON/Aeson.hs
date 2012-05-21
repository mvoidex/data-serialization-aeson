{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Data.Serialization.JSON.Aeson (
    FromObject, fromMember,
    ToObject, toMember,
    Jsonable,
    member,
    toJSON, fromJSON
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Aeson ((.:), (.=))
import Data.Serialization.Serialize
import Data.Serialization.Deserialize
import Data.Serialization.Serializable
import Data.Text

-- | Deserialize from object
newtype FromObject a = FromObject {
    fromObject :: StateT Aeson.Object Aeson.Parser a }
        deriving (Functor, Applicative, Alternative, Monad)

-- | Deserialize member
fromMember :: (Aeson.FromJSON a) => Text -> Deserialize FromObject a
fromMember name = Deserialize $ FromObject $ do
    obj <- get
    put (HM.delete name obj)
    lift $ obj .: name

instance Deserialization FromObject Aeson.Value where
    runDeserialization (FromObject f) (Aeson.Object v) = case Aeson.parse (evalStateT f) v of
        Aeson.Error s -> Left s
        Aeson.Success r -> Right r
    runDeserialization _ _ = Left "Not an object"
    deserializationEof _ = FromObject $ do
        obj <- get
        when (not $ HM.null obj) $ lift mzero
    deserializeTail = FromObject $ do
        obj <- get
        put $ HM.empty
        return $ Aeson.Object obj

-- | Serialize to object
newtype ToObject a = ToObject {
    toObject :: WriterT [Aeson.Pair] (Either String) a }
        deriving (Functor, Applicative, Alternative, Monad)

-- | Serialize member
toMember :: (Aeson.ToJSON a) => Text -> Serialize ToObject a
toMember name = Serialize $ \v -> ToObject $ tell [name .= v]

instance Serialization ToObject Aeson.Value where
    runSerialization (ToObject t) = Aeson.object <$> execWriterT t
    serializeTail (Aeson.Object v) = ToObject $ tell $ HM.toList v
    serializeTail _ = ToObject $ lift $ Left $ "Not an object"

-- | Type of serializable
type Jsonable a = Serializable Aeson.Value ToObject FromObject a

-- | Member of object
member :: (Aeson.ToJSON a, Aeson.FromJSON a) => Text -> Jsonable a
member name = serializable (toMember name) (fromMember name)

-- | ToJSON from Jsonable
toJSON :: Jsonable a -> a -> Aeson.Value
toJSON s = either onError id . encode s where
    onError s = error $ "Error on ToJSON from Jsonable: " ++ s

-- | FromJSON from Jsonable
fromJSON :: Jsonable a -> Aeson.Value -> Aeson.Parser a
fromJSON s (Aeson.Object v) = evalStateT (fromObject $ runDeserialize $ deserializer s) v
fromJSON _ _ = mzero
