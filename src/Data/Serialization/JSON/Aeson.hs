{-# LANGUAGE OverloadedStrings, DeriveGeneric, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, GeneralizedNewtypeDeriving, OverlappingInstances #-}

-- | Module implements serialization for JSON
--
-- Example of usage:
--
-- >data MyData = MyData {
-- >    myDataInt :: Int,
-- >    myDataString :: String }
-- >        deriving (Generic, Show)
-- >
-- >instance Serializable (Codec Aeson.Object ToObject FromObject) MyData
-- >
-- >myData :: JsonMemberable MyData
-- >myData = ser
-- >
-- >value = encode myData (MyData 0 "hello")
-- >-- Right fromList [("myDataInt",Number 0),("myDataString",String "Hello!")]
-- 
-- Objects can be nested:
-- 
-- >data MyData2 = MyData2 {
-- >    myData2String :: String,
-- >    myDataNested :: MyData }
-- >        deriving (Generic, Show)
-- >
-- >instance Serializable (Codec Aeson.Object ToObject FromObject) MyData2
-- >
-- >myData2 :: JsonMemberable MyData2
-- >myData2 = ser
-- >
-- >value2 = encode myData2 (MyData2 "foo" (MyData 10 "string"))
-- >-- Right fromList [("myDataNested",Object fromList [("myDataInt",Number 10),("myDataString",String "string")]),("myData2String",String "foo")]
-- 
-- And objects can be embedded within parent (so its fields placed in parent object directly). Note how @myData2String@ is placed in top object:
-- 
-- >data MyData3 = MyData3 {
-- >    myDataDouble :: Double,
-- >    myDataEmbed :: Embed MyData2 }
-- >        deriving (Generic, Show)
-- >
-- >instance Serializable (Codec Aeson.Object ToObject FromObject) MyData3
-- >
-- >myData3 :: JsonMemberable MyData3
-- >myData3 = ser
-- >
-- >value3 = encode myData3 (MyData3 0.4 (Embed (MyData2 "foo" (MyData 10 "string"))))
-- >-- Right fromList [("myDataDouble",Number 0.4),("myDataNested",Object fromList [("myDataInt",Number 10),("myDataString",String "string")]),("myData2String",String "foo")]
-- 
module Data.Serialization.JSON.Aeson (
    FromObject, FromValue, ToObject, ToValue,
    Jsonable, JsonMemberable,
    Embed(..),
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

import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Error
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Maybe (catMaybes, isJust)
import Data.Aeson ((.:), (.=))
import Data.Text as T
import Data.String (fromString)
import qualified Data.Vector as V
import GHC.Generics

import Data.Serialization.Combine
import Data.Serialization.Wrap
import Data.Serialization.Generic
import Data.Serialization.Codec
import Data.Serialization.Dictionary
import Data.Serialization.Combinators
import Data.Serialization.Text.Aeson

-- | Deserialize from object
newtype FromObject a = FromObject { runFromObject :: StateT Aeson.Object Aeson.Parser a }
    deriving (Functor, Applicative, Alternative, Monad, MonadState Aeson.Object, Generic)

instance MonadFail FromObject
instance GenericDecode FromObject where
    decodeStor name m = do
        v <- state (HM.lookup name' &&& HM.delete name')
        let
            subFields obj = case obj of
                Aeson.Object fs -> fs
                _ -> HM.singleton T.empty obj
        maybe (fail $ "Field " ++ name ++ " not found") (either fail return . deserialize m . subFields) v
        where
            name' = fromString name
instance Deserializer Aeson.Object FromObject where
    deserialize (FromObject p) s = case Aeson.parse (evalStateT p) s of
        Aeson.Error s -> Left s
        Aeson.Success r -> Right r
    deserializeEof _ = do
        obj <- get
        when (not $ HM.null obj) $ fail "EOF expected"
    deserializeTail = do
        obj <- get
        put HM.empty
        return obj

-- | Deserialize from value
newtype FromValue a = FromValue { runFromValue :: StateT (Maybe Aeson.Value) Aeson.Parser a }
    deriving (Functor, Applicative, Alternative, Monad, MonadState (Maybe Aeson.Value), Generic)

instance MonadFail FromValue
instance GenericDecode FromValue where
instance Deserializer Aeson.Value FromValue where
    deserialize (FromValue p) s = case Aeson.parse (evalStateT p) (Just s) of
        Aeson.Error e -> Left e
        Aeson.Success r -> Right r
    deserializeEof _ = do
        obj <- get
        when (isJust obj) $ fail "EOF expected"
    deserializeTail = do
        obj <- get
        put Nothing
        maybe (return Aeson.Null) return obj

-- | Serialize to object
newtype ToObject a = ToObject { runToObject :: EncodeTo [Aeson.Pair] a }
    deriving (Functor, Applicative, Alternative, Monad, MonadWriter [Aeson.Pair], MonadError String, Generic)

instance GenericEncode ToObject where
    encodeStor name m x = do
        v <- either throwError return $ serialize (m x)
        -- if v contains one element with empty name, serialize value as sub-object
        -- otherwise embed it
        case HM.toList v of
            [("", x)] -> tell [fromString name .= x]
            _ -> tell $ [fromString name .= Aeson.Object v]
instance Serializer Aeson.Object ToObject where
    serialize (ToObject p) = fmap HM.fromList $ encodeTo p
    serializeTail = tell . HM.toList

-- | Serialize to value
newtype ToValue a = ToValue { runToValue :: StateT (Maybe Aeson.Value) (Either String) a }
    deriving (Functor, Applicative, Alternative, Monad, MonadState (Maybe Aeson.Value), MonadError String, Generic)

instance GenericEncode ToValue
instance Serializer Aeson.Value ToValue where
    serialize (ToValue p) = maybe Aeson.Null id <$> execStateT p Nothing
    serializeTail v = do
        obj <- get
        maybe (put (Just v)) (const $ throwError "Can't serialize tail: json object is not null") obj

-- | Type of serializable
type Jsonable a = Codec Aeson.Value ToValue FromValue a

-- | Type of serializable as part of object
type JsonMemberable a = Codec Aeson.Object ToObject FromObject a

instance (Aeson.FromJSON a, Aeson.ToJSON a) => Serializable (Codec Aeson.Object ToObject FromObject) a where
    ser = member "" value

-- | Embed a used to tell serializer, that object must not be serialized as subobject, its fields must be placed directly in parent object instead.
data Embed a = Embed { unEmbed :: a }
    deriving (Generic, Show)

instance (Selector c, Serializable (Codec Aeson.Object ToObject FromObject) a) => GenericSerializable (Codec Aeson.Object ToObject FromObject) (Stor c (Embed a)) where
    gser = ser .:. Iso (unEmbed . unStor) (Stor . Embed)

-- | Deserialize member
fromMember_ :: (Aeson.FromJSON a) => Text -> Decoding FromObject a
fromMember_ name = Decoding $ do
    obj <- get
    put (HM.delete name obj)
    FromObject $ lift $ obj.: name

-- | Serialize member
toMember_ :: (Aeson.ToJSON a) => Text -> Encoding ToObject a
toMember_ name = encodePart $ \v -> Right [name .= v]

-- | Deserialize member with deserializer
fromMember :: Text -> Decoding FromValue a -> Decoding FromObject a
fromMember name s = Decoding $ do
    obj <- get
    put (HM.delete name obj)
    value <- FromObject $ lift $ obj .: name
    either fail return $ decode s (value :: Aeson.Value)

-- | Serialize member with serializer
toMember :: Text -> Encoding ToValue a -> Encoding ToObject a
toMember name s = encodePart $ fmap (\x -> [name .= (x :: Aeson.Value)]) . encode s

-- | Deserialize value
fromValue :: (Aeson.FromJSON a) => Decoding FromValue a
fromValue = Decoding $ do
    obj <- get
    put Nothing
    maybe (fail "EOF") (FromValue . lift . Aeson.parseJSON) obj

-- | Serialize value
toValue :: (Aeson.ToJSON a) => Encoding ToValue a
toValue = Encoding $ put . Just . Aeson.toJSON

-- | ToJSON from Jsonable
toJSON :: Jsonable a -> a -> Aeson.Value
toJSON s = either onError id . encode s where
    onError s = error $ "Error on ToJSON from Jsonable: " ++ s

-- | FromJSON from Jsonable
fromJSON :: Jsonable a -> Aeson.Value -> Aeson.Parser a
fromJSON s v = evalStateT (runFromValue $ runDecoding $ decoder s) (Just v)

-- | Member of object
member_ :: (Aeson.ToJSON a, Aeson.FromJSON a) => Text -> JsonMemberable a
member_ name = codec (toMember_ name) (fromMember_ name)

-- | Member of object with serializator
member :: Text -> Jsonable a -> JsonMemberable a
member name s = codec (toMember name $ encoder s) (fromMember name $ decoder s)

-- | Serialize object
object :: JsonMemberable a -> Jsonable a
object v = codec (toObject $ encoder v) (fromObject $ decoder v) where
    toObject :: Encoding ToObject a -> Encoding ToValue a
    toObject s = Encoding $ either throwError (put . Just . Aeson.Object) . encode s
    fromObject :: Decoding FromObject a -> Decoding FromValue a
    fromObject s = Decoding $ do
        obj <- get
        case obj of
            Just (Aeson.Object v) -> do
                put Nothing
                either fail return $ decode s v
            Just _ -> fail "JSON object expected"
            Nothing -> fail "JSON EOF"

-- | Serialize array of values
array :: Jsonable a -> Jsonable [a]
array v = codec (toArray $ encoder v) (fromArray $ decoder v) where
    toArray :: Encoding ToValue a -> Encoding ToValue [a]
    toArray s = Encoding $ either throwError (put . Just . Aeson.Array . V.fromList) . mapM (encode s)
    fromArray :: Decoding FromValue a -> Decoding FromValue [a]
    fromArray s = Decoding $ do
        obj <- get
        case obj of
            Just (Aeson.Array v) -> do
                put Nothing
                either fail return $ mapM (decode s) $ V.toList v
            Just _ -> fail "JSON array expected"
            Nothing -> fail "JSON EOF"

-- | Serialize any value
value :: (Aeson.ToJSON a, Aeson.FromJSON a) => Jsonable a
value = codec toValue fromValue

instance (Aeson.ToJSON a, Aeson.FromJSON a) => DictionaryValue Aeson.Value a where
    dictionaryValue = Convertible (encode value) (decode value)

-- | 'Jsonable' from 'Dictionarable'
dict :: Dictionarable Text Aeson.Value a -> Jsonable a
dict d = value <~> d
