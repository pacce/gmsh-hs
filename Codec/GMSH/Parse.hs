{-# LANGUAGE FlexibleContexts #-}

module Codec.GMSH.Parse () where

import Control.Applicative hiding ((<|>))
import Codec.GMSH.Types(Coordinate, Node(..))
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

nodes:: Stream s m Char => ParsecT s u m [Node]
nodes = do { _      <- string "$Nodes"
           ; _      <- endOfLine
           ; ns     <- natural
           ; _      <- endOfLine
           ; nodes  <- count ns nodeline
           ; _      <- string "$EndNodes"
           ; return nodes
           }
        where nodeline = node <* endOfLine

number :: Stream s m Char => ParsecT s u m [Char]
number = many1 digit

plus :: Stream s m Char => ParsecT s u m [Char]
plus = char '+' *> number
