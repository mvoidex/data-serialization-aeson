{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

-- | Module implements serialization for JSON
--
-- Example of usage:
--
-- @
-- data Some = Some Int Double [String]
-- 
-- $(makeIso \"some\" ''Some)
--
-- someJson :: Jsonable Some
-- someJson = object $
--   member "int" value .**.
--   member "double" value .**.
--   member "strings" (array value)
--   .:.
--   some
--
-- test :: encode someJson (Some 1 1.2 \"Hello\")
-- @
--
module Data.Serialization.JSON.Aeson (
    FromObject, FromValue, ToObject, ToValue,
    Jsonable, JsonMemberable,
    fromMember_, toMember_,
    fromMember, toMember,
    fromValue, toValue,
    toJSON, fromJSON,

    member_,
    member,

    object,
    array,
    value,

    dict,

    module Data.Serialization.Text.Aeson
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Maybe (catMaybes)
import Data.Aeson ((.:), (.=))
import Data.Serialization.Serialize
import Data.Serialization.Deserialize
import Data.Serialization.Serializable
import Data.Serialization.Combinators
import Data.Serialization.Dictionary
import Data.Text
import qualified Data.Vector as V

import Data.Serialization.Text.Aeson

-- | Deserialize from object
newtype FromObject a = FromObject {
    runFromObject :: StateT Aeson.Object Aeson.Parser a }
        deriving (Functor, Applicative, Alternative, Monad)

instance Deserialization FromObject Aeson.Object where
    runDeserialization (FromObject f) v = case Aeson.parse (evalStateT f) v of
        Aeson.Error s -> Left s
        Aeson.Success r -> Right r
    deserializationEof _ = FromObject $ do
        obj <- get
        when (not $ HM.null obj) $ lift mzero
    deserializeTail = FromObject $ do
        obj <- get
        put HM.empty
        return obj

-- | Deserialize from value
newtype FromValue a = FromValue {
    runFromValue :: StateT (Maybe Aeson.Value) Aeson.Parser a }
        deriving (Functor, Applicative, Alternative, Monad)

instance Deserialization FromValue Aeson.Value where
    runDeserialization (FromValue f) v = case Aeson.parse (evalStateT f) (Just v) of
        Aeson.Error s -> Left s
        Aeson.Success r -> Right r
    deserializationEof _ = FromValue $ do
        obj <- get
        maybe (return ()) (const $ lift mzero) obj
    deserializeTail = FromValue $ do
        obj <- get
        put Nothing
        maybe (return Aeson.Null) return obj

-- | Serialize to object
newtype ToObject a = ToObject {
    runToObject :: WriterT [Aeson.Pair] (Either String) a }
        deriving (Functor, Applicative, Alternative, Monad)

instance Serialization ToObject Aeson.Object where
    runSerialization (ToObject t) = HM.fromList <$> execWriterT t
    serializeTail v = ToObject $ tell $ HM.toList v

-- | Serialize to value
newtype ToValue a = ToValue {
    runToValue :: StateT (Maybe Aeson.Value) (Either String) a }
        deriving (Functor, Applicative, Alternative, Monad)

instance Serialization ToValue Aeson.Value where
    runSerialization (ToValue t) = maybe Aeson.Null id <$> execStateT t Nothing
    serializeTail v = ToValue $ do
        obj <- get
        maybe (put (Just v)) (const $ lift $ Left "Can't serialize tail: json object is not null") obj

-- | Type of serializable
type Jsonable a = Serializable Aeson.Value ToValue FromValue a

-- | Type of serializable as part of object
type JsonMemberable a = Serializable Aeson.Object ToObject FromObject a

-- | Deserialize member
fromMember_ :: (Aeson.FromJSON a) => Text -> Deserialize FromObject a
fromMember_ name = Deserialize $ FromObject $ do
    obj <- get
    put (HM.delete name obj)
    lift $ obj .: name

-- | Serialize member
toMember_ :: (Aeson.ToJSON a) => Text -> Serialize ToObject a
toMember_ name = Serialize $ \v -> ToObject $ tell [name .= v]

-- | Deserialize member with deserializer
fromMember :: Text -> Deserialize FromValue a -> Deserialize FromObject a
fromMember name s = Deserialize $ FromObject $ do
    obj <- get
    put (HM.delete name obj)
    value <- lift $ obj .: name
    lift $ evalStateT (runFromValue $ runDeserialize s) $ Just value

-- | Serialize member with serializer
toMember :: Text -> Serialize ToValue a -> Serialize ToObject a
toMember name s = Serialize $ \v -> ToObject $ do
    value <- lift $ execStateT (runToValue $ runSerialize s v) Nothing
    tell [name .= value]

-- | Deserialize value
fromValue :: (Aeson.FromJSON a) => Deserialize FromValue a
fromValue = Deserialize $ FromValue $ do
    obj <- get
    put Nothing
    maybe (lift $ fail "EOF") (lift . Aeson.parseJSON) obj

-- | Serialize value
toValue :: (Aeson.ToJSON a) => Serialize ToValue a
toValue = Serialize $ ToValue . put . Just . Aeson.toJSON

-- | ToJSON from Jsonable
toJSON :: Jsonable a -> a -> Aeson.Value
toJSON s = either onError id . encode s where
    onError s = error $ "Error on ToJSON from Jsonable: " ++ s

-- | FromJSON from Jsonable
fromJSON :: Jsonable a -> Aeson.Value -> Aeson.Parser a
fromJSON s v = evalStateT (runFromValue $ runDeserialize $ deserializer s) (Just v)

-- | Member of object
member_ :: (Aeson.ToJSON a, Aeson.FromJSON a) => Text -> JsonMemberable a
member_ name = serializable (toMember_ name) (fromMember_ name)

-- | Member of object with serializator
member :: Text -> Jsonable a -> JsonMemberable a
member name s = serializable (toMember name $ serializer s) (fromMember name $ deserializer s)

-- | Serialize object
object :: JsonMemberable a -> Jsonable a
object v = serializable (toObject $ serializer v) (fromObject $ deserializer v) where
    toObject :: Serialize ToObject a -> Serialize ToValue a
    toObject s = Serialize $ \v -> ToValue $ do
        v' <- lift $ runSerialization $ runSerialize s v
        put $ Just $ Aeson.Object v'
    fromObject :: Deserialize FromObject a -> Deserialize FromValue a
    fromObject s = Deserialize $ FromValue $ do
        obj <- get
        case obj of
            Just (Aeson.Object v) -> do
                put Nothing
                either (lift . fail) return $ runDeserialization (runDeserialize s) v
            Just _ -> lift $ fail "Not an object"
            Nothing -> lift $ fail "Nothing to deserialize"

-- | Serialize array of values
array :: Jsonable a -> Jsonable [a]
array v = serializable (toArray $ serializer v) (fromArray $ deserializer v) where
    toArray :: Serialize ToValue a -> Serialize ToValue [a]
    toArray s = Serialize $ \v -> ToValue $ do
        vs <- fmap catMaybes $ lift $ mapM ((`execStateT` Nothing) . runToValue . runSerialize s) v
        put $ Just $ Aeson.Array $ V.fromList vs
    fromArray :: Deserialize FromValue a -> Deserialize FromValue [a]
    fromArray s = Deserialize $ FromValue $ do
        obj <- get
        case obj of
            Just (Aeson.Array v) -> do
                mapM (either (lift . fail) return . runDeserialization (runDeserialize s)) (V.toList v)
            Just _ -> lift $ fail "Not an array"
            Nothing -> lift $ fail "Nothing to deserialize"

-- | Serialize any value
value :: (Aeson.ToJSON a, Aeson.FromJSON a) => Jsonable a
value = serializable toValue fromValue

instance (Aeson.ToJSON a, Aeson.FromJSON a) => DictionaryValue Aeson.Value a where
    dictionaryValue = Convertible (encode value) (decode value)

-- | 'Jsonable' from 'Dictionarable'
dict :: Dictionarable Text Aeson.Value a -> Jsonable a
dict d = value <~> d
