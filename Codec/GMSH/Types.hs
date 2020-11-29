module Codec.GMSH.Types (
        Coordinate,
        Node(..)
        ) where

type Coordinate = Float

data Node = Node {
    nodeNumber  :: Int,
    x           :: Coordinate,
    y           :: Coordinate,
    z           :: Coordinate
} deriving (Show)
