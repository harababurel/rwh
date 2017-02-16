module Prettify
    (
      Doc
    , empty
    , char
    , text
    , double
    , line
    , punctuate
    , hcat
    , fsep
    , (<>)
    ) where

import SimpleJSON
import Data.List (foldr1)

data Doc = Empty
         | Char Char
         | Text String
         | Line -- represents a line break
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Show, Eq)

-- Constructors
empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s  = Text s

double :: Double -> Doc
double x = text (show x)

line :: Doc
line = Line
-- /Constructors

fsep :: [Doc] -> Doc
fsep = foldr1 (</>)

(<>) :: Doc -> Doc -> Doc
Empty <> y     = y
x     <> Empty = x
x     <> y     = x `Concat` y

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line           = Char ' '
flatten (x `Union` _)  = flatten x
flatten other          = other

hcat :: [Doc] -> Doc
hcat = foldr1 (<>)

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []     = []
punctuate p [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds
