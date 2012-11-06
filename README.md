data-serialization-aeson
========================

Data serialization bindings for Aeson

Example of usage:

<pre>
data MyData = MyData {
    myDataInt :: Int,
    myDataString :: String }
        deriving (Generic, Show)

instance Serializable (Codec Aeson.Object ToObject FromObject) MyData

myData :: JsonMemberable MyData
myData = ser

value = encode myData (MyData 0 "hello")
-- Right fromList [("myDataInt",Number 0),("myDataString",String "Hello!")]
</pre>

Objects can be nested:

<pre>
data MyData2 = MyData2 {
    myData2String :: String,
    myDataNested :: MyData }
        deriving (Generic, Show)

instance Serializable (Codec Aeson.Object ToObject FromObject) MyData2

myData2 :: JsonMemberable MyData2
myData2 = ser

value2 = encode myData2 (MyData2 "foo" (MyData 10 "string"))
-- Right fromList [("myDataNested",Object fromList [("myDataInt",Number 10),("myDataString",String "string")]),("myData2String",String "foo")]
</pre>

And objects can be embedded within parent (so its fields placed in parent object directly). Note how <code>myData2String</code> is placed in top object:

<pre>
data MyData3 = MyData3 {
    myDataDouble :: Double,
    myDataEmbed :: Embed MyData2 }
        deriving (Generic, Show)

instance Serializable (Codec Aeson.Object ToObject FromObject) MyData3

myData3 :: JsonMemberable MyData3
myData3 = ser

value3 = encode myData3 (MyData3 0.4 (Embed (MyData2 "foo" (MyData 10 "string"))))
-- Right fromList [("myDataDouble",Number 0.4),("myDataNested",Object fromList [("myDataInt",Number 10),("myDataString",String "string")]),("myData2String",String "foo")]
</pre>
