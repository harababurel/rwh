module PutJSON where

import Data.List (intercalate)
import SimpleJSON

renderJValue :: JValue -> String
renderJValue (JString s)   = show s
renderJValue (JNumber x)   = show x
renderJValue (JBool True)  = "true"
renderJValue (JBool False) = "false"
renderJValue (JNull)       = "null"
renderJValue (JArray xs)   = "[" ++ intercalate ", " (map renderJValue xs) ++ "]"
renderJValue (JObject xs)  = "{" ++ intercalate "," (map processPair xs) ++ "}"
    where processPair (k, v) = show k ++ ": " ++ renderJValue v

putJValue :: JValue -> IO ()
putJValue x = putStrLn $ renderJValue x
