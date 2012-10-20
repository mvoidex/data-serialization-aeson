data-serialization-aeson
========================

Data serialization bindings for Aeson

Using example data from data-serialization, we can add serializer for Aeson:

<pre>
data Result s a =
    ValidResult s a |
    InvalidResult String (Maybe a) |
    UnknownResult [String]
        deriving (Eq, Ord, Read, Show)

$(makeIso "result" ''Result)
-- ^ result :: Iso (Result s a) (Either (s, a) (Either (String, Maybe a) [String]))

test :: (ToJSON s, FromJSON s, ToJSON a, FromJSON a) => Jsonable (Result s a)
test = object $
    (member_ "result" .**. member_ "state") .++.        -- ^ for ValidResult
    (member_ "error" .**. try (member_ "maybe")) .++.  -- ^ for InvalidResult
    (member_ "unknown")                                 -- ^ for UnknownResult
    .:.
    result                                            -- ^ Convert to Result s a
</pre>

and you can use it:

<pre>
test1 :: Either String Value
test1 = encode test (ValidResult 123 "blabla")
-- test1 = Right (Object (fromList [("state", String "blabla"), ("result", Number 123)]))

test2 :: Either String (Result Int String)
test2 = decode test (Object $ fromList [("error", String "blabla"), ("maybe", String "123")])
-- test2 = Right (InvalidResult "blabla" (Just "123")
</pre>

to encode/decode to ByteString:

<pre>
test3 :: Either String ByteString
test3 = encode (json &lt;~&gt; test) (ValidResult 123 "blabla")
-- test3 = "{\"state\":\"blabla\",\"result\":123}"
</pre>
