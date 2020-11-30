{-# LANGUAGE FlexibleContexts #-}

module Codec.GMSH.Parse () where

import Control.Applicative hiding ((<|>))
import Codec.GMSH.Types(
        Coordinate,
        Element(..),
        ElementType(..),
        Node(..)
        )
import Text.Parsec

(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b

coordinate :: Stream s m Char => ParsecT s u m Coordinate
coordinate = float

float :: Stream s m Char => ParsecT s u m Float
float = fmap rd $ integer <++> decimal <++> exponent
    where rd        = read :: String -> Float
          decimal   = option "" $ char '.' <:> number
          exponent  = option "" $ oneOf "eE" <:> integer

integer :: Stream s m Char => ParsecT s u m [Char]
integer = plus <|> minus <|> number

minus :: Stream s m Char => ParsecT s u m [Char]
minus = char '-' <:> number

natural :: Stream s m Char => ParsecT s u m Int
natural = fmap rd $ plus <|> number
    where rd = read :: String -> Int

number :: Stream s m Char => ParsecT s u m [Char]
number = many1 digit

plus :: Stream s m Char => ParsecT s u m [Char]
plus = char '+' *> number

-- Node Parse

nodes:: Stream s m Char => ParsecT s u m [Node]
nodes = do { _      <- string "$Nodes"
           ; _      <- endOfLine
           ; ns     <- natural
           ; _      <- endOfLine
           ; nodes  <- count ns (node <* endOfLine)
           ; _      <- string "$EndNodes"
           ; return nodes
           }

node :: Stream s m Char => ParsecT s u m Node
node = do { index <- natural
          ; _ <- spaces
          ; x <- coordinate
          ; _ <- spaces
          ; y <- coordinate
          ; _ <- spaces
          ; z <- coordinate
          ; return (Node index x y z)
          }

-- Element Parse

elements :: Stream s m Char => ParsecT s u m [Element]
elements =  do { _          <- string "$Elements"
               ; _          <- endOfLine
               ; es         <- natural
               ; _          <- endOfLine
               ; elements   <- count es (element <* endOfLine)
               ; _          <- string "$EndElements"
               ; return elements
               }

element :: Stream s m Char => ParsecT s u m Element
element = do { i        <- natural
             ; _        <- spaces
             ; (ts, es)
                    <-  elementParse (string "1") line
                    <|> elementParse (string "2") triangle
                    <|> elementParse (string "3") quadrangle
                    <|> elementParse (string "4") tetrahedron
                    <|> elementParse (string "5") hexahedron
                    <|> elementParse (string "8") line3
                    <|> elementParse (string "9") triangle6
             ; return (Element i ts es)
             }

elementParse :: Stream s m Char => (ParsecT s u m String) -> (ParsecT s u m ElementType) -> ParsecT s u m ([Int], ElementType)
elementParse elmtype elmval = do { _    <- elmtype
                                 ; _    <- spaces
                                 ; tags <- elementTag
                                 ; es   <- elmval
                                 ; return (tags, es)
                                 }

-- Element Tags Parse

elementTag :: Stream s m Char => ParsecT s u m [Int]
elementTag = do { ts   <- natural
                ; _    <- spaces
                ; tags <- count ts tagspace
                ; return tags
                }
            where tagspace = (fmap read $ integer) <* spaces

-- Element Type Parse

hexahedron :: Stream s m Char => ParsecT s u m ElementType
hexahedron = do { i0  <- natural
                ; _   <- spaces
                ; i1  <- natural
                ; _   <- spaces
                ; i2  <- natural
                ; _   <- spaces
                ; i3  <- natural
                ; _   <- spaces
                ; i4  <- natural
                ; _   <- spaces
                ; i5  <- natural
                ; _   <- spaces
                ; i6  <- natural
                ; _   <- spaces
                ; i7  <- natural
                ; return (Hexahedron i0 i1 i2 i3 i4 i5 i6 i7)
                }

line :: Stream s m Char => ParsecT s u m ElementType
line = do { i0  <- natural
          ; _   <- spaces
          ; i1  <- natural
          ; return (Line i0 i1)
          }

line3 :: Stream s m Char => ParsecT s u m ElementType
line3 = do { i0  <- natural
           ; _   <- spaces
           ; i1  <- natural
           ; _   <- spaces
           ; i2  <- natural
           ; return (Line3 i0 i1 i2)
           }

quadrangle :: Stream s m Char => ParsecT s u m ElementType
quadrangle = do { i0  <- natural
                ; _   <- spaces
                ; i1  <- natural
                ; _   <- spaces
                ; i2  <- natural
                ; _   <- spaces
                ; i3  <- natural
                ; return (Quadrangle i0 i1 i2 i3)
                }

tetrahedron :: Stream s m Char => ParsecT s u m ElementType
tetrahedron = do { i0  <- natural
                 ; _   <- spaces
                 ; i1  <- natural
                 ; _   <- spaces
                 ; i2  <- natural
                 ; _   <- spaces
                 ; i3  <- natural
                 ; return (Tetrahedron i0 i1 i2 i3)
                 }

triangle :: Stream s m Char => ParsecT s u m ElementType
triangle = do { i0  <- natural
              ; _   <- spaces
              ; i1  <- natural
              ; _   <- spaces
              ; i2  <- natural
              ; return (Triangle i0 i1 i2)
              }

triangle6 :: Stream s m Char => ParsecT s u m ElementType
triangle6 = do { i0  <- natural
               ; _   <- spaces
               ; i1  <- natural
               ; _   <- spaces
               ; i2  <- natural
               ; _   <- spaces
               ; i3  <- natural
               ; _   <- spaces
               ; i4  <- natural
               ; _   <- spaces
               ; i5  <- natural
               ; return (Triangle6 i0 i1 i2 i3 i4 i5)
               }
