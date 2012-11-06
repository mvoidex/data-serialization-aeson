-- | Module defines serializer for JSON to text
--
-- To serialize some value to text just chain 'json' and JSON-serializer for your date:
--
-- @
-- my :: Jsonable MyData
-- test = encode (json <~> my) (MyData 1 2 3)
-- @
--
module Data.Serialization.Text.Aeson (
    json
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Serialization
import Data.Serialization.Text.Print
import Data.Serialization.Text.Attoparsec

import qualified Data.Aeson as A

json :: CodecT ByteString Print Atto A.Value
json = codec (printWith (B.concat . L.toChunks . A.encode)) (atto A.json)
