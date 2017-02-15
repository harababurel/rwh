module SimpleJSON
    (
      JValue(..) -- export the type + all of its constructors
    , getString
    , getInt
    , getDouble
    , getBool
    , getArray
    , getObject
    , isNull
    ) where

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JArray [JValue]
            | JObject [(String, JValue)]
              deriving (Eq, Ord, Show)


getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _           = Nothing

getInt :: JValue -> Maybe Int
getInt (JNumber x) = Just (truncate x)
getInt _           = Nothing

getDouble :: JValue -> Maybe Double
getDouble (JNumber x) = Just x
getDouble _           = Nothing

getBool :: JValue -> Maybe Bool
getBool (JBool x) = Just x
getBool _         = Nothing

getArray :: JValue -> Maybe [JValue]
getArray (JArray xs) = Just xs
getArray _           = Nothing

getObject :: JValue -> Maybe [(String, JValue)]
getObject (JObject xs) = Just xs
getObject _            = Nothing

isNull :: JValue -> Bool
isNull x = x == JNull
