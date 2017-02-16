module PrettyJSON where

import Data.List (intercalate)
import Data.Bits (shitfR, .&.)
import Data.Char (ord)
import Numeric (showHex)
import SimpleJSON

renderJValue :: JValue -> Doc
renderJValue (JString s)   = string s
renderJValue (JNumber x)   = double x
renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
renderJValue (JNull)       = text "null"
-- renderJValue (JArray xs)   = "[" ++ intercalate ", " (map renderJValue xs) ++ "]"
-- renderJValue (JObject xs)  = "{" ++ intercalate ", " (map processPair xs) ++ "}"
    -- where processPair (k, v) = show k ++ ": " ++ renderJValue v

putJValue :: JValue -> IO ()
putJValue x = putStrLn $ renderJValue x

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
    Just r -> text r
    Nothing
        | mustEscape c -> hexEscape c
        | otherwise    -> char c
    where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where ch a b = (a, ['\\', b])

smallHex :: Int -> Doc
smallHex x  = text "\\u"
           <> text (replicate (4 - length h) '0')
           <> text h
    where h = showHex x ""

astral :: Int -> Doc
astral x = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
    where a = (n `shiftR` 10) .&. 0x3ff
          b = n .&. 0x3ff

hexEscape :: Char -> Doc
hexEscape c
    | d < 0x10000 = smallHex d
    | otherwise   = astral $ d - 0x10000
    where d = ord c

(<>) :: Doc -> Doc -> Doc
a <> b = undefined

char :: Char -> Doc
char c = undefined

hcat :: [Doc] -> Doc
hcat xs = undefined
