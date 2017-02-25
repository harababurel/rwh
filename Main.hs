module Main (main) where

import SimpleJSON
import PrettyJSON
import Prettify

main = print (JArray [JString "hello", JString "world"])
